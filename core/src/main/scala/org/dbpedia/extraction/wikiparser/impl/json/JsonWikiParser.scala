package org.dbpedia.extraction.wikiparser.impl.json

import org.dbpedia.extraction.sources.WikiPage
import org.dbpedia.extraction.wikiparser.{WikidataInterWikiLinkNode, Node, PageNode, WikiTitle,SimpleNode}

import net.liftweb.json._
import net.liftweb.json.JsonDSL._

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.util.matching.Regex
import org.dbpedia.extraction.destinations.{DBpediaDatasets, Quad}
import org.dbpedia.extraction.util.Language

import JsonWikiParser._


/**
 * Created with IntelliJ IDEA.
 * User: andread
 * Date: 08/04/13
 * Time: 14:22
 * To change this template use File | Settings | File Templates.
 */
object JsonWikiParser {
  /* the regex should search for languageslinks like "enwiki" only
  so that "enwikivoyage" for example wouldn't be acceptable because they wouldn't match a DBpedia entity
  */
  private val WikiLanguageRegex = """([^\s]+)wiki$""".r
}

class JsonWikiParser {

  implicit val formats = DefaultFormats

  def apply(page : WikiPage) : PageNode =
  {
    var nodes = getLanguageLinks(page)
    nodes = nodes ::: getLabels(page)
    nodes = nodes ::: getFacts(page)

    // Return page node
    new PageNode(page.title, page.id, page.revision, page.timestamp, page.contributorID, page.contributorName, false, false, nodes)
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

    /** get all nodes under json key  "links" which will be in the form
     *  {
     *   "arwiki": "نيويورك (مدينة)",
     *   "frwiki": "New York",
     *   "eowiki": "Novjorko",
     *   "plwiki": "Nowy Jork"
     *  }
     */
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

    nodes::= new SimpleNode(interLinksMap,null,SimpleNode.LanguageLinks)

    nodes
  }

  /**
   * Main functionality is parsing the WikiData Json page and extract labels in different languages the form
   * <http://www.w3.org/2000/01/rdf-schema#label> "New York City"@en
   *                                              "New York "@fr
   *                                              "New York"@co
   *@param page
   * @return SimpleObject that contains no UriTriples and it's valueTriples are filled with different labels on the form
   *         Labelproperty ->
   *                 lang -> label
   *
   *         <http://www.w3.org/2000/01/rdf-schema#label>  ->
   *                                                      "en" -> "New York City"
   *                                                      "fr" -> "New York"
   *                                                      "co" -> "New York"
   */
  def getLabels(page: WikiPage) : List[Node] = {

    var nodes = List[Node]()
    val json = page.source

    val parsedText = parseOpt(json)

    val jsonObjMap = parsedText match {
      case Some(map) => map
      case _ => throw new IllegalStateException("Invalid JSON representation!")
    }


    // get all nodes under json key  "label" which will be in the form
    //   {
    //    "en": "New York City",
    //    "ar": "مدينة نيو يورك",
    //    "fr": "New York",
    //    "it": "New York",
    //    "pl": "Nowy Jork",
    //  }
    // Json sample : http://pastebin.com/zygpzhJK

    val interLinks = (jsonObjMap \ "label") match {
      case JObject(links) => links
      case _ => List()
    }


    var labelsTriples = collection.mutable.Map[String, collection.mutable.Map[String,String]]()
    var labelsMap = collection.mutable.Map[String,String]()

    interLinks.foreach { field : JField =>

          val label :String = field.value.extract[String]
          val lang :String =  field.name
          labelsMap += lang -> label

    }

    labelsTriples += "http://www.w3.org/2000/01/rdf-schema#label" -> labelsMap

    nodes::= new SimpleNode(collection.mutable.Map.empty,labelsTriples,SimpleNode.Labels)

    nodes
  }


  /**
   * Main functionality is parsing the WikiData Json page and extract facts in different languages the form
   *
   * <http://www.w3.org/2000/01/rdf-schema#label> "New York City"@en
   *                                              "New York "@fr
   *                                              "New York"@co
   * @param page
   * @return SimpleObject that contains no UriTriples and it's valueTriples are filled with different labels on the form
   *         Labelproperty ->
   *                 lang -> label
   *
   *         <http://www.w3.org/2000/01/rdf-schema#label>  ->
   *                                                      "en" -> "New York City"
   *                                                      "fr" -> "New York"                                                        "co" -> "New York"
   */
  def getFacts(page: WikiPage) : List[Node] = {

    var nodes = List[Node]()
    val json = page.source

    val parsedText = parseOpt(json)

    val jsonObjMap = parsedText match {
      case Some(map) => map
      case _ => throw new IllegalStateException("Invalid JSON representation!")
    }


    /** get all nodes under json key  "claims" which will be in the form
    *Json sample : http://pastebin.com/9H6s2Nid
    */

    /** scenario is as following :
      * 1- check that   m has "value" not some value or no value
      * 2- check that it's "rank":1
      * 3- check for the third item in the claim
      *   a- string  > write as it is
      *   b- time >  take time property of the 4th item "time":"+00000001931-03-03T00:00:00Z" and it's type would be xsd:datetime
      *   c- globe coordinate > change them to DBpedia point(lat long)
      *   d- common media > relpace spaces with _ and add "http://commons.wikimedia.org/wiki/File:" to begining of it and it's datatype is null
      *   e- wikibase-entityid : get entity id  /numeric-id  and add "http://wikipeida.dbpedia.org/resource/Q" to it
      *
      * 4- depending on the output type decide to add it to the URITriples or ValuesTriples
      */

    var valueTriples = collection.mutable.Map[String, collection.mutable.Map[String,String]]()
    var URITriples = collection.mutable.Map[String, List[String]]()


    //get claims only whose are values and has rank ==1 in List[JObject]

    val claims = for {
      JObject(claim) <- (jsonObjMap \ "claims")
      JField("rank", JInt(rank)) <- claim
      JArray(m) <- (claim \ "m")
      if rank == 1 && m(0).extract[String] == "value"
    } yield claim



    for (claim <- claims)
    {
      val values = collection.mutable.Map[String,String]()
      var Uris =  List[String]()
      val propID = (claim \ "m")(1).extract[Int]
      val property = "http://www.wikidata.org/entity/P"+propID


      (claim \ "m")(2).extract[String] match {

        case "wikibase-entityid" =>
        {
          Uris ::= "http://wikidata.dbpedia.org/resource/Q"+((claim \ "m")(3) \ "numeric-id").extract[Int]
          URITriples +=  property -> Uris
        }

        case "string" =>
        {
          if(isCommonMediaFiles("P"+propID))
          {
            val value = "http://commons.wikimedia.org/wiki/File:" + (claim \ "m")(3).extract[String].replace(" ","_")    // "" empty datatype means no datatype for URIs and URLs
            values +=  value -> "CommonMediaFile"
            valueTriples +=  property -> values
          }
          else
          {
            values += (claim \ "m")(3).extract[String] -> ""
            valueTriples +=  property -> values
          }
        }


        case "time" =>
        {
          values += ((claim \ "m")(3)\ "time").extract[String] -> "xsd:date"
          valueTriples +=  property -> values
        }
        case "globecoordinate" =>
        {
          val lat = ((claim \ "m")(3)\ "latitude").extract[Int]
          val long = ((claim \ "m")(3)\ "longitude").extract[Int]

          values +=  lat +" "+long -> ""
          valueTriples +=  property -> values
        }
        case _=>

      }

    }

    //nodes::= new SimpleNode()
    nodes::= new SimpleNode(URITriples,valueTriples,SimpleNode.Facts)
    nodes
  }


  //helper function for checking the type of property , used in getFacts method
  def isCommonMediaFiles(prop:String) :Boolean = {
    val commonMediaFilesProperties = List("P10","P109","P117","P14","P15","P154","P158","P18","P181","P207","P242","P367","P368","P41","P443","P491","P51","P623","P692","P94")
    commonMediaFilesProperties.contains(prop)
  }




}

