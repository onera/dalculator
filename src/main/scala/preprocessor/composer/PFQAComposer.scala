package preprocessor.composer

import dalculator.model.SeverityLevel
import dalculator.model.SeverityLevel.Unknown
import dalculator.utils.FileManager

import java.io.{File, FileWriter}
import scala.io.Source
import scala.xml.{Node, NodeSeq, XML, Source => XMLSource}

object PFQAComposer {

  final val SEPARATOR = ", "

  private case class Event(equipment: String, component: String, failureMode: String) {
    override def toString: String = s"$equipment$SEPARATOR$component$SEPARATOR$failureMode"
  }

  sealed trait MergeMethod {
    def merge(s:Seq[String]):String
  }
  case object CollapseMerge extends MergeMethod {
    def merge(s:Seq[String]):String =
      s.mkString(" and ")
  }
  case class OrderedMerge(order:Seq[String]) extends MergeMethod{
    def merge(s:Seq[String]):String = {
      val missing = s.filter(x => !order.contains(x))
      if (missing.nonEmpty)
        println(s"[WARNING] the order ${order.mkString(" < ")} provided for dictionary does not contain ${missing.mkString(", ")}")
      s.minBy(s => order.indexOf(s))
    }

  }

  case class ColumnInfo(name:String, isFLowDictionary: Boolean, map:Map[String,Seq[String]], merge:Option[MergeMethod] = None)

  private object Event {
    def empty: Event = Event("", "", "")

    def apply(s: String): Option[Event] = {
      val split = s.split("\\.")
      if (split.size >= 4)
        Some(new Event(split.head, split.tail.init.mkString("."), split.last))
      else if (split.size == 3)
        Some(new Event(split.head, split(1), split.last))
      else if (split.size == 2)
        Some(new Event(split.head, "", split.last))
      else {
        println(s"[WARNING] $s is not fulfilling format constraints")
        None
      }
    }
  }

  private case class TableLine(event: Event, columns:Seq[Seq[String]]) {
    override def toString: String = {
        columns.foldLeft(Seq(s"$event"))((acc,values) =>
          for {
            a <- acc
            v <- values
          } yield
            a + s"$SEPARATOR$v"
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
                               reformatName: String => String,
                               columns: Seq[ColumnInfo]): Seq[TableLine] = for {
    line <- lines
    event <- line \ "@evt"
    e <- Event(reformatName(event.toString()))
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
    TableLine(e, finalValues)
  }

  final def performAndExportPFQA(
                                  fileName: String,
                                  outputFile:String,
                                  reformatName: String => String = x => x,
                                  columns: Seq[ColumnInfo],
                                  filterEvents: Node => Boolean =  _ => true
                                ): File = {
    val reader = XMLSource.fromFile(fileName)
    val result = XML.load(reader)

    val analysisLines = (result \\ "tr").filter(n => (n \ "@evt").exists(filterEvents))

    val table = buildTable(analysisLines, reformatName, columns)
    val output = FileManager.analysisDirectory.getFile(outputFile)
    val writer = new FileWriter(output)
    writer.write(s"Equipment${SEPARATOR}Component${SEPARATOR}Failure mode")
    for {c <- columns}
      writer.write(s"${SEPARATOR}${c.name}")
    writer.write("\n")
    for {line <- table}
      writer.write(line.toString)
    writer.close()
    output
  }
}
