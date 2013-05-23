package com.dadrox.scuttle.tz

import java.io.File
import scala.io.BufferedSource

case class ZoneContinuation(offset: String, rules: String, format: String, untilYear: Option[String])

case class Zone(name: String, offset: String, rules: String, format: String, untilYear: Option[String])
case class Rule(name: String, from: String, to: String, typ: String, in: String, on: String, at: String, save: String, letter: String)
case class Link(to: String, from: String)

case class TimeZoneData(zones: IndexedSeq[Zone], rules: IndexedSeq[Rule], links: IndexedSeq[Link]) {
    lazy val size = zones.size + rules.size + links.size

    lazy val description = "Rules\n" + rules.mkString("\n") + "Zones\n" + zones.mkString("\n") + "Links\n" + links.mkString("\n")
}

class OlsonTimeZoneDataParser {

    val CommentLine = """#.*""".r
    val CommentInLine = """(.*)#.*""".r
    val BlankLine = """\s*""".r
    val ZoneLine = """^Zone\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)(\s+.+)?\s*$""".r
    val ZoneContinuationLine = """\s+(\S+)\s+(\S+)\s+(\S+)(\s+.+)?\s*$""".r
    val RuleLine = """Rule\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s*$""".r
    val LinkLine = """Link\s+(\S+)\s+(\S+)\s*$""".r

    private def parseLine(line: String): Option[Any] = line match {
        case CommentLine() | BlankLine()                             => None
        case CommentInLine(data)                                     => parseLine(data) // recurse
        case ZoneLine(name, offset, rules, format, until)            => Some(Zone(name, offset, rules, format, Option(until).map(_.trim)))
        case ZoneContinuationLine(offset, rules, format, until)      => Some(ZoneContinuation(offset, rules, format, Option(until).map(_.trim)))
        case RuleLine(name, from, to, typ, in, on, at, save, letter) => Some(Rule(name, from, to, typ, in, on, at, save, letter))
        case LinkLine(to, from)                                      => Some(Link(to, from))
        case skip =>
            println("SKIPPED " + skip)
            None
    }

    def parseData(data: List[String]): TimeZoneData = {
        @annotation.tailrec
        def loop(zones: IndexedSeq[Zone], rules: IndexedSeq[Rule], links: IndexedSeq[Link], currentZoneName: String, data: List[String]): TimeZoneData = data match {
            case Nil => TimeZoneData(zones, rules, links)
            case line :: tail => parseLine(line) match {
                case Some(z: Zone)              => loop(zones :+ z, rules, links, z.name, tail)
                case Some(zc: ZoneContinuation) => loop(zones :+ Zone(currentZoneName, zc.offset, zc.rules, zc.format, zc.untilYear), rules, links, currentZoneName, tail)
                case Some(link: Link)           => loop(zones, rules, links :+ link, "", tail)
                case Some(rule: Rule)           => loop(zones, rules :+ rule, links, "", tail)
                case None                       => loop(zones, rules, links, "", tail)
            }
        }
        loop(IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty, null, data)
    }

    def parse(source: BufferedSource, path: String) = {

        val data = parseData(source.getLines.toList)

        val zones = data.zones
        val rules = data.rules
        val links = data.links

        println(path)
        //        println(data.size + " lines of actual data")
        //        println(zones.size + " zones")
        //        println(rules.size + " rules")
        //        println(links.size + " links")
        //        println

        //                println(data.description)
        println("ZONES=\n" + zones.mkString("\n"))
        //        println("RULES=\n" + rules.sortBy(_.name).mkString("\n"))
        //        println("LINKS=\n" + links.mkString("\n"))
        //        println(rules.groupBy(_.name).map { case (k, v) => k -> v.size }.toList.sortBy(_._1).mkString("\n"))
    }
    def source(file: File) = io.Source.fromFile(file)
    def source(path: String) = io.Source.fromFile(path)

    def fromDirectory(path: String) = {
        val dir = new File(path)
        if (!dir.isDirectory()) throw new IllegalArgumentException("The path provided must be a directory: " + path)
        else if (!dir.canRead()) throw new IllegalArgumentException("The directory provided must be readable: " + path)
        dir.listFiles().toList.flatMap { file =>
            file.getName() match {
                case "factory"                       => None
                case name if (name.endsWith(".tab")) => None
                case _                               => Some(file)
            }
        } foreach { file =>
            parse(source(file), file.getAbsolutePath())
        }
    }
}

object Main extends App {
    val parser = new OlsonTimeZoneDataParser()
    parser.fromDirectory("/home/cwood/code/git/scuttle/scuttle-tz/src/main/resources/tzdata/")

    //    parser.parse("/home/cwood/code/git/scuttle/scuttle-tz/src/main/resources/tzdata/northamerica")
    //    parser.parse("/home/cwood/code/git/scuttle/scuttle-tz/src/main/resources/tzdata/africa")
    //    parser.parse("/home/cwood/code/git/scuttle/scuttle-tz/src/main/resources/tzdata/antarctica")
    //    parser.parse("/home/cwood/code/git/scuttle/scuttle-tz/src/main/resources/tzdata/backward")
    //    parser.parse("/home/cwood/code/git/scuttle/scuttle-tz/src/main/resources/tzdata/etcetera")
    //    parser.parse("/home/cwood/code/git/scuttle/scuttle-tz/src/main/resources/tzdata/europe")
    //    parser.parse("/home/cwood/code/git/scuttle/scuttle-tz/src/main/resources/tzdata/pacificnew")
    //    parser.parse("/home/cwood/code/git/scuttle/scuttle-tz/src/main/resources/tzdata/solar87")
    //    parser.parse("/home/cwood/code/git/scuttle/scuttle-tz/src/main/resources/tzdata/solar88")
    //    parser.parse("/home/cwood/code/git/scuttle/scuttle-tz/src/main/resources/tzdata/solar89")
    //    parser.parse("/home/cwood/code/git/scuttle/scuttle-tz/src/main/resources/tzdata/systemv")
}