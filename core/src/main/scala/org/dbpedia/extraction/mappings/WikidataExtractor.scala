package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.ontology.Ontology
import org.dbpedia.extraction.util.Language
import org.dbpedia.extraction.destinations.{Quad, DBpediaDatasets}
import org.dbpedia.extraction.wikiparser.{WikidataInterWikiLinkNode, Namespace, PageNode}
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
    // Filter the namespace we want to parse
    //if(page.title.namespace != Namespace.Main) return Seq.empty

    // This is the JSON we want to parse.
    // May contain JSON escaped characters like \u042F
    val json = page.toWikiText

    // We now need to parse the json content. This is a sample of what can be found
    // http://pastebin.com/zygpzhJK

    // This array will hold all the triples we will extract
    val quads = new ArrayBuffer[Quad]()



    // This is how we add new triples
    //quads += new Quad(context.language, DBpediaDatasets.Labels, subjectUri, labelProperty, label, page.sourceUri, context.ontology.datatypes("xsd:string"))

    // return the list
    //quads



    for (node <- page.children) {

        quads += new Quad(context.language, DBpediaDatasets.Wikidata, subjectUri, "<http://www.w3.org/2002/07/owl#sameAs>", "Sample_xxaaffgd" , page.sourceUri,null)

    }

    quads
  }
}
