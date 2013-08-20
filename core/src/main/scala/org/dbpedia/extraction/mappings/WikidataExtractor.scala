package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.destinations.{Quad, DBpediaDatasets}
import org.dbpedia.extraction.wikiparser.{SimpleNode, WikidataInterWikiLinkNode, Namespace, PageNode}
import collection.mutable.ArrayBuffer

/**
 * Extracts data from Wikidata sources.
 * This is a copy of WikiPageExtractor for now with comments
 */
class WikidataExtractor(
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
  //private val sameasProperty = context.ontology.properties("")


  // this is where we will store the output
  override val datasets = Set(DBpediaDatasets.Wikidata)

  override def extract(page : PageNode, subjectUri : String, pageContext : PageContext): Seq[Quad] =
  {

    // http://pastebin.com/zygpzhJK

    // This array will hold all the triples we will extract
    val quads = new ArrayBuffer[Quad]()

    //in the languagelinks case simplenode contains 1 output
    for (n <- page.children) { // was page.children.reverse - why?
      n match {
        case node: SimpleNode => {

          for (property <- node.getTriples.keys)
          {
            for( llink <- node.getTriples(property))
            {
              for (llink2 <- node.getTriples(property) diff List(llink))
              {
                quads += new Quad(context.language, DBpediaDatasets.Wikidata, llink, property,llink2, page.sourceUri,null)
              }
            }
          }

        }

        case _=>

    }
    }

    quads
  }
}
