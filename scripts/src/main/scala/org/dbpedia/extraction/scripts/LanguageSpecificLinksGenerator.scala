//package org.dbpedia.extraction.scripts

import java.io._
import scala.collection.immutable.HashMap
import scala.io.Source
import scala.io._
import sys.process._


/**
 * User: hadyelsahar
 *
 */


object LanguageSpecificLinksGenerator {

  /**
   * HashMap to keep track of all opened files BufferedWriters in order to close and flush them
   * when needed
   */
  var filesWriters = new HashMap[String,BufferedWriter]()

  /**
   * Helper function to split .nt file lines and extract subject , pred , object and the fullstop
   * @param arg triple line
   * @return array of sub , pred, obj and fullstop
   */
  private def split(arg: String): Array[String] = {
    arg.split(" ").map(_.trim).filter(_.nonEmpty)
  }

  /**
   * helper function to create files and save them in te filesWriter HashMap
   * @param fileName
   */
  private def CreateFile(fileName: String)
  {
    val file = new File(fileName)
    file.createNewFile()
    val writer = new BufferedWriter(new FileWriter(file))
    filesWriters += (fileName->writer)
  }

  /**
   * helper function to write line by line in a file
   * @param file name of the file as created by the CreateFile Function and saved in the HashMap
   * @param str string to be written in the file
   */
  private def LogToFile(file: String, str: String)
  {
    val writer = filesWriters(file)
    writer.write(str)
    writer.newLine()
  }

  /**
   * destructive function to flush and close all opened buffered writers
   */
  private def CloseWriters ()
  {
    for(w <- filesWriters)
    {
      w._2.flush()
      w._2.close()
    }
  }



  def main(args: Array[String]) {
    //todo : add some requires here to check for arguments
    //arg1 = 0
    val option = args(0)

     /**
      * option 0 :
      * -----------
      *extracting language links related properties from the WikiData RDF Dumb File
      * and save them in a separated languagelinks.nt file
       */
    if(option == "0")
    {
      val baseDir = new File(args(1))
      val file = Source.fromFile(baseDir)

      CreateFile("./languagelinks.ttl")

      for(ln <- file.getLines){
        val triple = split(ln);

        //check if the triple is in the correct .ttl format
        if(triple.length ==4){

          //languagelinks triples needed are those contain schema:about predicates and wikipediapages subjects which indicated wikipedia page
          val Regx = """.*\.wikipedia.org\/wiki.*<http:\/\/schema\.org\/about>""".r

          if(Regx.findFirstIn(ln) != None ){
            triple(0) = triple(0).replace(".wikipedia.org/wiki",".dbpedia.org/resource")
            LogToFile("./languagelinks.nt",triple(2)+" "+"<http://www.w3.org/2002/07/owl#sameAs>"+" "+triple(0)+" .")
          }

        }

      }

      CloseWriters()
    }

    /**
     * option 1:
     * ---------
     * from the extracted languagelinks.nt file
     * extracting language links and save them in languagelinks folder
     */
    if(option == "1")
    {
      val baseDir = new File(args(1))
      val file = Source.fromFile(baseDir)


      for(ln <- file.getLines){
        var triple = split(ln);

        if(triple.length ==4){
          val Q1 = triple(0)
          val Obj1 = triple(2)

          val langRegx = """<http:\/\/(.*).dbpedia.org\/resource\/.*>""".r
          val langRegx(lang) = triple(2)

          //make folder for ll files
          new File("./llinkfiles").mkdir()

          //create languagefile for each language if doesn't exist before
          val LLFileName = "./llinkfiles/interlanguage_links_same_as_"+lang+".ttl"

          if(!filesWriters.contains(LLFileName))
          {
            CreateFile(LLFileName)
          }

          //iterate over all triples todo: change to more efficient way
          for(ln <- Source.fromFile(baseDir).getLines){
            triple = split(ln);

            if(triple.length ==4 ){
              val Q2= triple(0)
              val Obj2 = triple(2)
              val langRegx(innerLang) = triple(2)

              if(lang != innerLang && Q1 == Q2)
              {
                LogToFile(LLFileName,Obj1+" "+"<http://www.w3.org/2002/07/owl#sameAs>"+" "+Obj2+" .")
              }
            }
          }
        }
      }
      CloseWriters()
    }


  }

}

