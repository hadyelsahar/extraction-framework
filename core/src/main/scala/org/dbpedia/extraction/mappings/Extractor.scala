package org.dbpedia.extraction.mappings

import org.dbpedia.extraction.destinations.Quad
import org.dbpedia.extraction.wikiparser._
import org.dbpedia.extraction.sources.WikiPage

/**
 * Extractors are mappings that extract data from a PageNode.
 * Necessary to get some type safety in CompositeExtractor: 
 * Class[_ <: Extractor] can be checked at runtime, but Class[_ <: Mapping[PageNode]] can not.
 */
trait Extractor[T]extends Mapping[T] {

  //Type of class that each extractor accepts  PageNode , WikiPages ..etc
    val Type : Extractor.Type

}

////Enum to show different types that Extractor should accept
//// Todo : remove String match of Type and replace with check on T (Type Arugment using TypeTag )
object Extractor extends Enumeration {
  type Type = Value
  val PageNodeType = Value("PageNode")
  val WikiPageType = Value("WikiPage")
  //Any for composite Extractors
  val Any = Value("Any")
}
