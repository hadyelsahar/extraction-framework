package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.destinations.Quad
import org.dbpedia.extraction.wikiparser.PageNode
import org.dbpedia.extraction.mappings.Extractor
import org.dbpedia.extraction.sources.{WikiPage}

class CompositeExtractor[T](extractors: Seq[Extractor[T]])
extends CompositeMapping[T](extractors: _*)
with Extractor[T]
{
  val Type = Extractor.Any
}

/**
 * Creates new extractors.
 */
object CompositeExtractor
{
    /**
     *
     * @param classes List of Extractors to be initialized Might be of Different Types
     * @param context Any type of object that implements the required parameter methods for the extractors
     * @return new CompositeExtractor that is loaded with Extractors and ParseExtractors
     */
    def loadToParsers(classes : Seq[Class [_ <:Extractor[_]]], context : AnyRef) : CompositeExtractor[WikiPage] = {

      val extractors = classes.map(_.getConstructor(classOf[AnyRef]).newInstance(context))

      //define different types of Extractors
      var wikiPageExtractors =Seq[Extractor[WikiPage]]()
      var pageNodeExtractors =Seq[Extractor[Any]]()
      var jsonNodeExtractors =Seq[Extractor[Any]]()
      //to do: add json extractors

      extractors foreach { extractor =>
        extractor.Type match {
          case Extractor.WikiPageType =>  wikiPageExtractors  = wikiPageExtractors :+ extractor.asInstanceOf[Extractor[WikiPage]]           //select all extractors which take Wikipage to wrap them in a CompositeExtractor
          case Extractor.PageNodeType =>  pageNodeExtractors  = pageNodeExtractors :+ extractor.asInstanceOf[Extractor[Any]]           //select all extractors which take PageNode to wrap them in WikiParseExtractor
          case Extractor.JsonNodeType =>  jsonNodeExtractors  = jsonNodeExtractors :+ extractor.asInstanceOf[Extractor[Any]]
          case _ =>
        }
      }

      val wikipageCompositeExtractor = new CompositeExtractor[WikiPage](wikiPageExtractors)

      //create and load WikiParseExtractor here
      val wikiParseExtractor = new WikiParseExtractor(pageNodeExtractors)

      //create and load JsonParseExtractor here
      val jsonParseExtractor = new JsonParseExtractor(jsonNodeExtractors)

      //collect ParseExtractors and CompositeExtractor
      val allExtractors = Seq[Extractor[WikiPage]](wikiParseExtractor,jsonParseExtractor) ++ wikiPageExtractors

      new CompositeExtractor[WikiPage](allExtractors)
    }

    /**
     * Creates a new CompositeExtractor loaded with same type of Extractors[T]
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
