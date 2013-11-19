package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.destinations.Quad
import org.dbpedia.extraction.wikiparser.PageNode
import org.dbpedia.extraction.mappings.Extractor

class CompositeExtractor[T](extractors: Seq[Extractor[T]])
extends CompositeMapping[T](extractors: _*)
with Extractor[T]

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
     * @param classes List of extractor classes to be instantiated
     * @param context Any type of object that implements the required parameter methods for the extractors
     */
    def load[T](classes: Seq[Class[_ <: Extractor[_]]], context: AnyRef): Extractor[T] =
    {
      val extractors = classes.map(_.getConstructor(classOf[AnyRef]).newInstance(context))

      val selectedExtractors : Seq[Extractor[T]] = Seq.empty

      extractors foreach { extractor : Extractor[_] =>
        extractor match {
        case ex : Extractor[T] =>  selectedExtractors :+ ex
        case _ =>
        }
      }

      new CompositeExtractor[T](selectedExtractors: Seq[Extractor[T]])
    }
}
