package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.destinations.{Quad, DBpediaDatasets}
import org.dbpedia.extraction.wikiparser.{SimpleNode, PageNode}
import collection.mutable.ArrayBuffer
import org.dbpedia.extraction.ontology.io.OntologyReader

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
  override val datasets = Set(DBpediaDatasets.WikidataFacts)

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
            //check for triples that doesn't contain Label or sameas properties only
//              if(property != "http://www.w3.org/2000/01/rdf-schema#label" && property != "http://www.w3.org/2002/07/owl#sameAs" ){
//
//                val valueFacts = node.getValueTriples(property)
//                for( fact <- valueFacts.keys)
//                {
//                    quads += new Quad(Language.apply("en"), DBpediaDatasets.WikidataFacts, subjectUri, property ,fact , page.sourceUri, context.ontology.datatypes("xsd:string"))
//                }
//              }
          }

          //Generating Quads for Uri
          for (property <- node.getUriTriples.keys)
          {
            //check for triples that doesn't contain Label or sameas properties only
            if(property != "http://www.w3.org/2000/01/rdf-schema#label" && property != "http://www.w3.org/2002/07/owl#sameAs" ){

              //labels are in the form of valuesTriples so SimpleNode.getValueTriples method is used  which returns Map[String,String]
              val UriFacts = node.getUriTriples(property)
              for( fact <- UriFacts)
              {
                //print(context.ontology.equivalentPropertiesMap.size)

                context.ontology.equivalentPropertiesMap.foreach({map =>
                  if (map._1.toString.matches(property))
                  {
                    map._2.foreach{mappedProp =>
                    quads += new Quad(Language.apply("en"), DBpediaDatasets.WikidataFacts, subjectUri, mappedProp.toString,fact , page.sourceUri,null)
                    }
                  }
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
}


