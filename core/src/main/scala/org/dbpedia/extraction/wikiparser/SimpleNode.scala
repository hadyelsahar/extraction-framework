package org.dbpedia.extraction.wikiparser

import org.dbpedia.extraction.sources.WikiPage

/**
 * it's a class which returns string triples in a Map in order to bypass that AST can't describe all triples
 * in which the key will be the property and the value will be the list of values for the property
 * the subject will be provided to the Extractor
 * todo: fix the extraction framework to accept new dataypes
 *
 * @param triples  the list of properties and objects for specific subject page
 *
 * @param children The contents of this page
 */

class SimpleNode (
  val triples: collection.mutable.Map[String, List[String]] = collection.mutable.Map.empty,
  children: List[Node] = List.empty
) 
extends Node(children, 0)
{
    def getTriples : collection.mutable.Map[String,List[String]] = triples
    def toPlainText : String = getTriples.mkString
    def toWikiText : String = getTriples.mkString
}
