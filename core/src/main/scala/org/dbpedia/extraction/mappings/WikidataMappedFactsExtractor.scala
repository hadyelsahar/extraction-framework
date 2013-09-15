package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.ontology.{OntologyProperty, Ontology}
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.destinations.{Quad, DBpediaDatasets}
import org.dbpedia.extraction.wikiparser.{SimpleNode, PageNode , TextNode}
import collection.mutable.ArrayBuffer
import org.dbpedia.extraction.ontology.io.OntologyReader
import org.dbpedia.extraction.dataparser.{DataParser, DateTimeParser}
import org.dbpedia.extraction.ontology.datatypes.Datatype

/**
 * Extracts Wikidata claims
 * on the form of
 * value triples:
 * <http://wikidata.dbpedia.org/resouce/Q64> <http://www.wikidata.org/entity/P625> "33.3333333 -123.433333333"
 * URI triples
 * <http://wikidata.dbpedia.org/resouce/Q64> <http://www.wikidata.org/entity/P625> <wikidata.dbpedia.org/resource/Q223>
 *
 */
class WikidataMappedFactsExtractor(
                         context : {
                           def ontology : Ontology
                           def redirects : Redirects // redirects required by DateTimeParser
                           def language : Language
                         }
                         )
  extends Extractor
{
  // Here we define all the ontology predicates we will use
  private val isPrimaryTopicOf = context.ontology.properties("foaf:isPrimaryTopicOf")
  private val primaryTopic = context.ontology.properties("foaf:primaryTopic")
  private val dcLanguage = context.ontology.properties("dc:language")
  private val labelProperty = context.ontology.properties("rdfs:label")


  // this is where we will store the output
  override val datasets = Set(DBpediaDatasets.WikidataMappedFacts)

  override def extract(page : PageNode, subjectUri : String, pageContext : PageContext): Seq[Quad] =
  {
    // This array will hold all the triples we will extract
    val quads = new ArrayBuffer[Quad]()

    //for each parser method exists a children node , you can differentiate between them through the Tiples property
    //for example :  skos:label  >> for labels extractor
    //               owl:sameas >> for  Language links
    for (n <- page.children) {

      n match {
        case node: SimpleNode => {

          //Generating Quads for ValueTriples
          for (property <- node.getValueTriples.keys)
          {
            val valueFacts = node.getValueTriples(property)
            for( fact <- valueFacts.keys)
            {
            //check for triples that doesn't contain Label or sameas properties only
              node.NodeType match {
                case SimpleNode.CoordinatesFacts => {
                  quads += new Quad(null, DBpediaDatasets.WikidataMappedFacts, subjectUri, context.ontology.properties(property) ,fact , page.sourceUri)
                }

                case SimpleNode.CommonMediaFacts => {
                  //map the property to equivalent one //to do make helper function for getting equivalent list of properties
                  // take into consideration that dataTypes of properties should be URI not strings
                  getDBpediaSameasProperties(property).foreach{dbProp =>

                    val fileURI = "http://commons.wikimedia.org/wiki/File:" + fact.replace(" ","_")
                    quads += new Quad(context.language, DBpediaDatasets.WikidataMappedFacts, subjectUri, dbProp.uri,fileURI , page.sourceUri,null)

                  }
                }

                case SimpleNode.StringFacts =>{
                  //lot of parsing has to be done depending on data-type categories
                  getDBpediaSameasProperties(property).foreach{dbProp =>

                    quads += new Quad(null, DBpediaDatasets.WikidataMappedFacts, subjectUri,dbProp ,fact , page.sourceUri)

                  }
                }

                case SimpleNode.TimeFacts =>{

                  getDBpediaSameasProperties(property).foreach{dbProp =>

                    val dateParser = new DateTimeParser(context, dbProp.range.asInstanceOf[Datatype])
                    dateParser.parse(new TextNode(fact,0)) match {
                      case Some(date) => quads += new Quad(context.language, DBpediaDatasets.WikidataMappedFacts, subjectUri, dbProp,date.toString, page.sourceUri)
                      case None =>
                    }
                  }
                }

                case _ =>

              }
            }
          }

          //Generating Quads for Uri and Replace Wikidata property with DBpedia mapped one
          for (property <- node.getUriTriples.keys)
          {
            //check for triples that doesn't contain Lpropertyabel or sameas properties only
            if(node.NodeType == SimpleNode.Facts || node.NodeType == SimpleNode.MappedFacts){

              //labels are in the form of valuesTriples so SimpleNode.getValueTriples method is used  which returns Map[String,String]
              val UriFacts = node.getUriTriples(property)
              for( fact <- UriFacts)
              {
                    getDBpediaSameasProperties(property).foreach({mappedProp =>
                      quads += new Quad(Language.apply("en"), DBpediaDatasets.WikidataMappedFacts, subjectUri, mappedProp.toString,fact , page.sourceUri,null)
                    })
              }
            }
          }
        }

        case _ =>

    }
    }

    quads
  }


  def getDBpediaSameasProperties(property:String) : Set[OntologyProperty] =
  {
    var properties = Set[OntologyProperty]()
    context.ontology.equivalentPropertiesMap.foreach({map =>
      if (map._1.toString.matches(property))
      {
        map._2.foreach{mappedProp =>
          properties += mappedProp
        }
      }
    })

  properties
  }

  //Mappings Parsers to DBpedia Properties
  //todo: replace it in the extraction framework

//  def getParserType (datatypeURI :String) : DataParser = {
//
//
//
//  }




}


