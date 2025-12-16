package preprocessor.composer

import dalculator.model.SeverityLevel
import dalculator.model.SeverityLevel.Unknown
import dalculator.utils.FileManager

import java.io.{File, FileWriter}
import scala.io.Source
import scala.xml.{Node, NodeSeq, XML, Source => XMLSource}

object PFQAComposer {

  final val SEPARATOR = ", "

  sealed trait MergeMethod {
    def merge(s:Seq[String]):String
  }
  case class CollapseMerge(separator:Option[String]) extends MergeMethod {
    def merge(s:Seq[String]):String =
      s.mkString(s" ${separator.getOrElse("and")} ")
  }
  case class OrderedMerge(order:Seq[String]) extends MergeMethod{
    def merge(s:Seq[String]):String = {
      val missing = s.filter(x => !order.contains(x))
      if (missing.nonEmpty)
        println(s"[WARNING] the order ${order.mkString(" < ")} provided for dictionary does not contain ${missing.mkString(", ")}")
      s.minBy(s => order.indexOf(s))
    }

  }

  case class ColumnInfo(name:String, isFLowDictionary: Boolean, map:Map[String,Seq[String]], merge:Option[MergeMethod] = None) {
    override def toString: String = name
  }

  private case class TableLine(columns:Seq[Seq[String]]) {
    override def toString: String = {
        columns.foldLeft(Seq(""))((acc,values) =>
          for {
            a <- acc
            v <- values
          } yield {
            if(a.nonEmpty)
              a + s"$SEPARATOR$v"
            else
              v
          }
        ).mkString("","\n","\n")
    }
  }

  final def getDictionary(file: String): Option[(String,Map[String, Seq[String]])] = {
    val source = Source.fromFile(file)
    val result =
      (for {
        l <- source.getLines()
        elements = l.split(",").toList
        if elements.size >= 2
      } yield {
        elements.head -> elements.tail
      }).toSeq
    source.close()
    for{
      (_,l) <- result.headOption
      title <- l.headOption
    } yield
      (title, result.tail.toMap)
  }

  private final def buildTable(lines: NodeSeq,
                               columns: Seq[ColumnInfo]): Seq[TableLine] = for {
    line <- lines
    event <- line \ "@evt"
    flows = (line \ "flow").filter(n => (n \ "@value").exists(_.toString().contains("true")))
    names = (flows \\ "@name").map(_.toString())
  } yield {
    val finalValues = {
      for {
        c <- columns
      } yield {
        val values =
          if(c.isFLowDictionary) {
            c.map.filter(p => names.contains(p._1))
          } else
            c.map.filter(p => p._1 == event.toString())
        if (values.isEmpty)
          Seq("None")
        else {
          val valuesForC = values.values.flatten.toSeq.distinct
          c.merge match {
            case Some(merge) => Seq(merge.merge(valuesForC))
            case None => valuesForC
          }
        }
      }
    }
    TableLine(finalValues)
  }

  final def performAndExportPFQA(
                                  fileName: String,
                                  outputFile:String,
                                  columns: Seq[ColumnInfo],
                                  filterEvents: Node => Boolean =  _ => true
                                ): File = {
    val reader = XMLSource.fromFile(fileName)
    val result = XML.load(reader)

    val analysisLines = (result \\ "tr").filter(n => (n \ "@evt").exists(filterEvents))

    val table = buildTable(analysisLines, columns)
    val output = FileManager.analysisDirectory.getFile(outputFile)
    val writer = new FileWriter(output)
    writer.write(columns.mkString(SEPARATOR))
    writer.write("\n")
    for {line <- table}
      writer.write(line.toString)
    writer.close()
    output
  }
}
