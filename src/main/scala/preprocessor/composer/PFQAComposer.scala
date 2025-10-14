package preprocessor.composer

import dalculator.utils.FileManager

import java.io.{File, FileWriter}
import scala.io.Source
import scala.xml.{Node, NodeSeq, XML, Source => XMLSource}

object PFQAComposer {

  final val SEPARATOR = ", "

  private case class Event(equipment: String, component: String, failureMode: String) {
    override def toString: String = s"$equipment$SEPARATOR$component$SEPARATOR$failureMode"
  }

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

  private case class TableLine(event: Event, severity: String, alarms: Set[String], mode: String, law: String, failureCondition: String) {
    override def toString: String = alarms match {
      case s if s.isEmpty => s"$event$SEPARATOR$law$SEPARATOR$severity$SEPARATOR$mode${SEPARATOR}None$SEPARATOR$failureCondition\n"
      case s =>
        (for{ alarm <- s.toSeq.sorted}
          yield
            s"$event$SEPARATOR$law$SEPARATOR$severity$SEPARATOR$mode$SEPARATOR$alarm$SEPARATOR$failureCondition").mkString("","\n","\n")
    }
  }

  final def getDictionary(file: String): Map[String, Seq[String]] = {
    val source = Source.fromFile(file)
    val result =
      (for {
        l <- source.getLines()
        elements = l.split(",").toList
        if elements.size >= 2
      } yield {
        elements.head -> elements.tail
      }).toMap
    source.close()
    result
  }

  private final def buildTable(lines: NodeSeq,
                               events: Map[Event, Seq[String]],
                               reformatName: String => String,
                               toAction: Map[String, String],
                               toAlarm: Map[String, Seq[String]],
                               toFC: Map[String, String],
                               toSeverity: Map[String, String]): Seq[TableLine] = for {
    line <- lines
    event <- line \ "@evt"
    flows = (line \ "flow").filter(n => (n \ "@value").exists(_.toString().contains("true")))
    names = (flows \\ "@name").map(_.toString())
    alarms = names.collect(toAlarm).flatten.toSet
    severity = names.collect(toSeverity)
    modes = names.collect(toAction)
    failureConditions = names.collect(toFC)
    severity <- if (severity.isEmpty) Seq("Unknown") else severity
    mode = if (modes.isEmpty) "None" else modes.distinct.mkString(" & ")
    fc = if(failureConditions.isEmpty) "None" else failureConditions.distinct.mkString(" and ")
    e <- Event(reformatName(event.toString()))
  } yield {
    val law = events.get(e) match {
      case None => "N/A"
      case Some(s) if s.isEmpty => "N/A"
      case Some(s) => s.head
    }
    TableLine(e, severity, alarms, mode, law, fc)
  }

  final def performAndExportPFQA(
                                  fileName: String,
                                  outputFile:String,
                                  reformatName: String => String = x => x,
                                  toAction: Map[String, String],
                                  toAlarm: Map[String, Seq[String]],
                                  toFC: Map[String, String],
                                  toSeverity: Map[String, String],
                                  filterEvents: Node => Boolean =  _ => true
                                ): File = {
    val reader = XMLSource.fromFile(fileName)
    val result = XML.load(reader)
    val events = {
      for {
        event <- result \\ "event"
        law <- event \ "law"
        name <- event \ "@name"
        e <- Event(reformatName(name.toString()))
      } yield {
        e -> (law \ "@value").map(_.toString())
      }
    }.toMap

    val analysisLines = (result \\ "tr").filter(n => (n \ "@evt").exists(filterEvents))

    val table = buildTable(analysisLines, events, reformatName, toAction, toAlarm, toFC, toSeverity)

    val output = FileManager.analysisDirectory.getFile(outputFile)
    val writer = new FileWriter(output)
    writer.write(s"Equipment${SEPARATOR}Component${SEPARATOR}Failure mode${SEPARATOR}Law${SEPARATOR}Criticality${SEPARATOR}Recovery action${SEPARATOR}Detection mean(s)${SEPARATOR}Failure condition(s)\n")
    for {line <- table}
      writer.write(line.toString)
    writer.close()
    output
  }
}
