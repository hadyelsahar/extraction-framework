//package org.dbpedia.extraction.scripts

import java.io._
import scala.collection.immutable.HashMap
import scala.io.Source
import scala.io._
import sys.process._


/**
 * User: hadyelsahar
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
            LogToFile("./languagelinks.ttl",triple(2)+" "+"<http://www.w3.org/2002/07/owl#sameAs>"+" "+triple(0)+" .")
          }

        }

      }

      CloseWriters

    }

    /**
     * option 1:
     * ---------
     * from the extracted languagelinks.nt file
     * extracting language links and save them in languagelinks folder
     */
    if(option == "1")
    {
      val startTime = System.nanoTime

      //opening master file for language links
      val baseDir = new File(args(1))
      val file = Source.fromFile(baseDir)

      //creating folder for output files
      new File("./llinkfiles").mkdir()

      var Q = ""
      var oldQ = ""
      var triplesObjects = List[String]()
      val lines = file.getLines
      for(ln <- lines){

        val triple = split(ln);

        //gather all objects of triples until the subject changes
          oldQ = Q
          Q = triple(0)
          val tripleObj = triple(2)

        //for each chuck ( the subject changed or if it's the last line ) , make combinations and save to files

          if((oldQ != Q && oldQ != "") || !lines.hasNext)
          {
            //println(oldQ)
            for(obj <- triplesObjects)
            {
              //extracting language
              val langRegx = """<http:\/\/(.*).dbpedia.org\/resource\/.*>""".r
              val langRegx(lang) = obj

              //creating file for language if not exists
              val fileName = "./llinkfiles/interlanguage_links_same_as_"+lang+".ttl"

              if(!filesWriters.contains(fileName))
              {
                CreateFile(fileName)
              }

              //creating combination string
              var LLString :String= ""
              //removing itself
              val innerTripleObjects = triplesObjects.diff(List(obj))


              for(obj2 <- innerTripleObjects)
              {
                //LLString += obj +" <http://www.w3.org/2002/07/owl#sameAs> " +obj2+" .\n"
                LogToFile(fileName,obj +" <http://www.w3.org/2002/07/owl#sameAs> " +obj2+" .\n")
              }

              //LogToFile(fileName,LLString)
            }

            //empty the Chunk container
            triplesObjects = List[String]()
          }

          triplesObjects = triplesObjects :+ tripleObj
        }

      CloseWriters()

      print("time taken: " + (System.nanoTime - startTime)/1000000000 +" secs" )


    }


  }


}

