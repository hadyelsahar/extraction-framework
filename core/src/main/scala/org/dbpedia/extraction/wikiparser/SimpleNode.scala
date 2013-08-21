package org.dbpedia.extraction.wikiparser

import org.dbpedia.extraction.sources.WikiPage

/**
 * it's a class which returns string triples in a Map in order to bypass that AST can't describe all triples
 *
 * the subject will be provided to the Extractor
 * Holder for two kinds of triples
 * 1- Uritriples : in which the key will be the property and the value will be the list of URI values for the property
 * 2- valueTriples : in which the key will be the property and the value will be the Map of values for the property in the form Value:lang
 * to store languages also beside the values
 *
 * when building the extractor user should know which kind of triples he is expecting values or uri ones
 * or handle both cases if it's unexpectable
 *
 * todo: fix the extraction framework to accept new dataypes
 *
 * @param uriTriples  the list of properties and objects for specific subject page
 *
 * @param children The contents of this page
 */

class SimpleNode (
  val uriTriples: collection.mutable.Map[String, List[String]] = collection.mutable.Map.empty,
  val valueTriples: collection.mutable.Map[String, collection.mutable.Map[String,String]] = collection.mutable.Map.empty,
  children: List[Node] = List.empty
) 
extends Node(children, 0)
{
    def getUriTriples : collection.mutable.Map[String,List[String]] = uriTriples
    def getValueTriples : collection.mutable.Map[String,collection.mutable.Map[String,String]] = valueTriples
    def toPlainText : String = getUriTriples.mkString+getValueTriples.mkString
    def toWikiText : String = getUriTriples.mkString+getValueTriples.mkString
}
