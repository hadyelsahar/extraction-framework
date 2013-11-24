package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.destinations.{Dataset, Quad}
import org.dbpedia.extraction.sources.{WikiPageFormat, WikiPage}
import org.dbpedia.extraction.wikiparser.{PageNode, WikiParser}
import org.dbpedia.extraction.sources.WikiPageFormat.WikiPageFormat
import org.dbpedia.extraction.wikiparser.impl.simple.SimpleWikiParser
import org.dbpedia.extraction.wikiparser.impl.json.JsonWikiParser

/**
 * User: hadyelsahar
 * Date: 11/19/13
 * Time: 12:43 PM
 *
 * ParseExtractors as explained in the design : https://f.cloud.github.com/assets/607468/363286/1f8da62c-a1ff-11e2-99c3-bb5136accc07.png
 * A ParsingExtractor parses a WikiPage and passes the result to the next extractor. For each format, we adapt the object by sending the format as input .
 *
 * to prevent creation of new subclasses for each datatype , proper parser is automatically selected in this class
 * Given the WikiPageFormat
 *
 * @param format format of pages that this ParseExtractor will parse (WikiText , Json ..etc )
 * @param mappings  Sequence of next level Extractors
 *
 * */
 class ParseExtractor(format : WikiPageFormat , mappings: Seq[Extractor[Any]])extends Extractor[WikiPage]{

  val Type = Extractor.WikiPageType

  override val datasets: Set[Dataset] = mappings.flatMap(_.datasets).toSet

  override def extract(input: WikiPage , subjectUri: String, context: PageContext): Seq[Quad] = {

    input.asInstanceOf[WikiPage].format match {
      case format => {
        format match {
          case WikiPageFormat.WikiText =>
            val parser= new SimpleWikiParser()
            mappings.flatMap(_.extract(parser(input), subjectUri, context))
          case WikiPageFormat.Json =>
            val parser = new JsonWikiParser()
            mappings.flatMap(_.extract(parser(input), subjectUri, context))
          case _ => Seq.empty

        }
      }

      }

    }

}
