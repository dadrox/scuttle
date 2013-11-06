package com.dadrox.scuttle.tz

import Io._
import scala.io.BufferedSource
import scala.util.matching.Regex
import com.dadrox.scuttle.string._
import com.dadrox.scuttle.time._

case class ZoneContinuation(offset: Option[Duration], rules: Option[String], format: Option[String], untilYear: Option[String])

case class Zone(name: String, offset: Option[Duration], rules: Option[String], format: Option[String], untilYear: Option[String])
case class Rule(name: String, from: Int, to: Int, typ: Option[String], in: String, on: String, at: String, save: Duration, letters: Option[String])
case class Link(to: String, from: String)

case class TimeZoneData(zones: IndexedSeq[Zone], rules: IndexedSeq[Rule], links: IndexedSeq[Link]) {
    lazy val size = zones.size + rules.size + links.size

    lazy val description = "Rules\n" + rules.mkString("\n") + "Zones\n" + zones.mkString("\n") + "Links\n" + links.mkString("\n")

    def ++(other: TimeZoneData) = TimeZoneData(zones ++ other.zones, rules ++ other.rules, links ++ other.links)
}

/** Parses the tz data avilable at ftp://ftp.iana.org/tz/releases/
 *  http://www.twinsun.com/tz/tz-link.htm for reference
 */
class OlsonTimeZoneDataParser {
    def fromDirectory(path: String) = {
        val data = Io.directory(path).listFiles().toList.flatMap { file =>
            file.getName() match {
                case "factory"                         => None
                case "leapseconds"                     => None
                case "solar87" | "solar88" | "solar89" => None
                case name if (name.endsWith(".tab"))   => None
                case _                                 => Some(file)
            }
        } map { file =>
            new OlsonTimeZoneDataFileParser(file).parse
        }
        data.foldLeft(TimeZoneData(IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty)) { _ ++ _ }
    }
}

object OlsonTimeZoneDataFileParser {
    object RuleParser {
        val RuleLine = """Rule\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s*$""".r
        val SaveField = """(\d{1,2}):(\d\d)""".r

        def unapply(s: String): Option[Rule] = s match {
            case RuleLine(name, from, to, typ, in, on, at, save, letters) =>
                Some(Rule(name, YearFrom(from), YearTo(to, from), Type(typ), in, on, at, Save(save), Letters(letters)))
            case _ => None
        }

        def YearFrom(from: String): Int = from match {
            case "min" => Int.MinValue
            case year  => year.toInt
        }
        def YearTo(to: String, from: String): Int = to match {
            case "only" => YearFrom(from)
            case "max"  => Int.MaxValue
            case year   => year.toInt
        }
        def Type(in: String): Option[String] = in match {
            case "-" => None
            case t   => Some(t)
        }
        def Letters(in: String): Option[String] = in match {
            case "-" => None
            case t   => Some(t)
        }
        def Save(in: String): Duration = in match {
            case IsInt(i)        => i.hour
            case SaveField(h, m) => h.toInt.hours + m.toInt.minutes
        }
    }

    object ZoneParser {
        val ZoneLine = """^Zone\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)(\s+.+)?\s*$""".r
        val OffsetHMS = """(-)?(\d{1,2}):(\d{1,2}):(\d{2})""".r
        val OffsetHM = """(-)?(\d{1,2}):(\d{2})""".r

        def unapply(s: String): Option[Zone] = s match {
            case ZoneLine(name, offset, rules, format, until) => Some(Zone(name, Offset(offset), Rules(rules), Format(format), until.notBlank.map(_.trim)))
            case _                                            => None
        }
        def Format(in: String): Option[String] = in match {
            case "LMT"  => None
            case format => Some(format)
        }
        def Rules(in: String): Option[String] = in match {
            case "-"  => None
            case rule => Some(rule)
        }
        def Offset(in: String): Option[Duration] = in match {
            case IsInt(i)                    => Some(Duration.fromHours(i))
            case OffsetHMS("-", h, m, s)     => Some(-duration(h, m, s))
            case OffsetHMS("+" | _, h, m, s) => Some(duration(h, m, s))
            case OffsetHM("-", h, m)         => Some(-duration(h, m))
            case OffsetHM("+" | _, h, m)     => Some(duration(h, m))
            case _                           => None
        }

        private def duration(h: String, m: String): Duration = h.trim.toInt.hours + m.trim.toInt.minutes
        private def duration(h: String, m: String, s: String): Duration = duration(h, m) + s.trim.toInt.seconds
    }

    object ZoneContinuationParser {
        val ZoneContinuationLine = """\s+(\S+)\s+(\S+)\s+(\S+)(\s+.+)?\s*$""".r

        def unapply(s: String): Option[ZoneContinuation] = s match {
            case ZoneContinuationLine(offset, rules, format, until) =>
                Some(ZoneContinuation(ZoneParser.Offset(offset), ZoneParser.Rules(rules), ZoneParser.Format(format), until.notBlank.map(_.trim)))
            case _ => None
        }
    }

    val CommentLine = """#.*""".r
    val CommentInLine = """(.*)#.*""".r
    val BlankLine = """\s*""".r
    val LinkLine = """Link\s+(\S+)\s+(\S+)\s*$""".r
}

class OlsonTimeZoneDataFileParser(file: java.io.File) {
    val path = file.getAbsolutePath()
    val source = Io.source(file)

    import OlsonTimeZoneDataFileParser._

    private def parseLine(line: String): Option[Any] = line match {
        case CommentLine() | BlankLine() => None
        case CommentInLine(data)         => parseLine(data) // recurse
        case ZoneParser(zone)            => Some(zone)
        case ZoneContinuationParser(zc)  => Some(zc)
        case RuleParser(rule)            => Some(rule)
        case LinkLine(to, from)          => Some(Link(to, from))
        case skip =>
            println("SKIPPED " + skip)
            None
    }

    private def parseData(data: List[String]): TimeZoneData = {
        @annotation.tailrec
        def loop(zones: IndexedSeq[Zone], rules: IndexedSeq[Rule], links: IndexedSeq[Link], currentZoneName: String, data: List[String]): TimeZoneData = data match {
            case Nil => TimeZoneData(zones, rules, links)
            case line :: tail => parseLine(line) match {
                case Some(z: Zone)              => loop(zones :+ z, rules, links, z.name, tail)
                case Some(zc: ZoneContinuation) => loop(zones :+ Zone(currentZoneName, zc.offset, zc.rules, zc.format, zc.untilYear), rules, links, currentZoneName, tail)
                case Some(link: Link)           => loop(zones, rules, links :+ link, "", tail)
                case Some(rule: Rule)           => loop(zones, rules :+ rule, links, "", tail)
                case None                       => loop(zones, rules, links, currentZoneName, tail)
            }
        }
        loop(IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty, null, data)
    }

    def parse() = parseData(source.getLines.toList)
}

object Main extends App {
    val parser = new OlsonTimeZoneDataParser()
    val data = parser.fromDirectory("/home/cwood/code/git/scuttle/scuttle-tz/src/main/resources/tzdata/")

    val zones = data.zones
    val rules = data.rules
    val links = data.links

    println(data.size + " lines of actual data")
    //        println(zones.size + " zones")
    //        println(rules.size + " rules")
    //        println(links.size + " links")
    //        println

    //    println(data.description)
    println("ZONES=\n" + zones.sortBy(_.name).mkString("\n"))
    //    println("RULES=\n" + rules.sortBy(_.name).mkString("\n"))
    //            println("LINKS=\n" + links.mkString("\n"))
    //        println(rules.groupBy(_.name).map { case (k, v) => k -> v.size }.toList.sortBy(_._1).mkString("\n"))
}