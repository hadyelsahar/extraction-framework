package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.destinations.Quad
import org.dbpedia.extraction.wikiparser.PageNode
import org.dbpedia.extraction.mappings.Extractor

class CompositeExtractor(extractors: Seq[Extractor[PageNode]])
extends CompositeMapping[PageNode](extractors: _*)
with Extractor[PageNode]

/**
 * Creates new extractors.
 */
object CompositeExtractor
{
    /**
     * Creates a new extractor.
     *
     * TODO: using reflection here loses compile-time type safety.
     *
     * @param extractors List of extractor classes to be instantiated
     * @param context Any type of object that implements the required parameter methods for the extractors
     */
    def load(classes: Seq[Class[_ <: Extractor[_]]], context: AnyRef): Extractor[PageNode] =
    {
      val extractors = classes.map(_.getConstructor(classOf[AnyRef]).newInstance(context))

      var pageNodeExtractors : Seq[Extractor[PageNode]] = Seq.empty

      extractors foreach { extractor : Extractor[_] =>
        extractor match {
        case ex : Extractor[PageNode] =>  pageNodeExtractors :+ ex
        case _ =>
      }
      }

      new CompositeExtractor(pageNodeExtractors: Seq[Extractor[PageNode]])
    }
}
