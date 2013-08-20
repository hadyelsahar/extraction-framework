package org.dbpedia.extraction.wikiparser.impl.json

import org.dbpedia.extraction.sources.WikiPage
import org.dbpedia.extraction.wikiparser.{WikidataInterWikiLinkNode, Node, PageNode, WikiTitle,SimpleNode}
import net.liftweb.json.DefaultFormats
import net.liftweb.json.JsonParser._
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.util.matching.Regex
import org.dbpedia.extraction.destinations.{DBpediaDatasets, Quad}
import org.dbpedia.extraction.util.Language

import JsonWikiParser._
import net.liftweb.json.JsonAST._

/**
 * Created with IntelliJ IDEA.
 * User: andread
 * Date: 08/04/13
 * Time: 14:22
 * To change this template use File | Settings | File Templates.
 */

object JsonWikiParser {
  private val WikiLanguageRegex = """([^\s]+)wiki""".r
}

class JsonWikiParser {

  implicit val formats = DefaultFormats

  def apply(page : WikiPage) : PageNode =
  {
    val nodes = getLanguageLinks(page)
    // Return page node
    new PageNode(page.title, page.id, page.revision, page.timestamp, page.contributorID, page.contributorName, false, false, nodes)
  }

  def collectInterLanguageLinks(page: WikiPage) : List[Node] = {

    var nodes = List[Node]()
    val json = page.source

    val parsedText = parseOpt(json)

    val jsonObjMap = parsedText match {
      case Some(map) => map
      case _ => throw new IllegalStateException("Invalid JSON representation!")
    }

    val interLinks = (jsonObjMap \ "links") match {
      case JObject(links) => links
      case _ => List()
    }

    val interLinksMap = collection.mutable.Map[String, String]()

    interLinks.foreach { interLink : JField =>
      interLink.name match {
        case WikiLanguageRegex(lang) =>  interLinksMap += lang -> interLink.value.extract[String]
        case _ =>
      }
    }

    if (! interLinksMap.contains("en")) return nodes

    val sourceTitle = WikiTitle.parse(interLinksMap.get("en").get, Language.English)
    // Do not generate a link to the defaul language itself
    interLinksMap -= "en"

    interLinksMap.foreach {
      case (key, value) =>
        Language.map.get(key) match {
          case Some(lang) =>
            val destinationTitle = WikiTitle.parse(key + ":" + value, lang)
            nodes ::= WikidataInterWikiLinkNode(sourceTitle, destinationTitle)
          case _ =>
        }
      case _ =>
    }

    nodes
  }


  /**
   * Main functionality is parsing the WikiData Json page and extract language links related tripleson the form
   * Subject  <http://www.w3.org/2002/07/owl#sameAs> <dbpedia.org/resource/New_York>
    *@param page
   * @return
   */
  def getLanguageLinks(page: WikiPage) : List[Node] = {

    var nodes = List[Node]()
    val json = page.source

    val parsedText = parseOpt(json)

    val jsonObjMap = parsedText match {
      case Some(map) => map
      case _ => throw new IllegalStateException("Invalid JSON representation!")
    }

    // get all nodes under json key  "links" which will be in the form
    //   {
    //    "arwiki": "نيويورك (مدينة)",
    //    "frwiki": "New York",
    //    "eowiki": "Novjorko",
    //    "plwiki": "Nowy Jork"
    //  }
    val interLinks = (jsonObjMap \ "links") match {
      case JObject(links) => links
      case _ => List()
    }


    var interLinksMap = collection.mutable.Map[String, List[String]]()
    var values = List[String]()

    interLinks.foreach { interLink : JField =>
      interLink.name match {
        //use regex to remove the convert  arwiki -> ar

        case WikiLanguageRegex(lang) =>  {
          var wikiPageName :String = interLink.value.extract[String]
          val suffix = wikiPageName.replace(" ","_")
          val prefix = if (lang=="en") "" else lang+"."

          values ::= "http://"+prefix+"dbpedia.org/resource/"+suffix+""
        }
        case _ =>
      }
    }

    interLinksMap += "http://www.w3.org/2002/07/owl#sameAs" -> values

    nodes::= new SimpleNode(interLinksMap)

    nodes
  }


}

