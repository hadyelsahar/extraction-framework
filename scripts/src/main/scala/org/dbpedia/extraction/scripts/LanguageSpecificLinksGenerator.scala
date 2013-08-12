package org.dbpedia.extraction.scripts

import java.io._
import scala.collection.immutable.HashMap
import scala.io._
import org.dbpedia.util.text.uri._
import org.dbpedia.extraction.util.RichFile.wrapFile
import org.dbpedia.extraction.util.RichReader.wrapReader
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import org.apache.commons.compress.compressors.bzip2.{BZip2CompressorInputStream, BZip2CompressorOutputStream}
import org.dbpedia.extraction.util.FileLike
import java.nio.charset.Charset

/**
 * User: hadyelsahar
 */

object LanguageSpecificLinksGenerator {

  //Todo: remove and include from org.dbpedia.extraction.util.IOUtils when merging Dump branch to main
    object IOUtils {

    /**
     * Map from file suffix (without "." dot) to output stream wrapper
     */
    val zippers = Map[String, OutputStream => OutputStream] (
      "gz" -> { new GZIPOutputStream(_) },
      "bz2" -> { new BZip2CompressorOutputStream(_) }
    )

    /**
     * Map from file suffix (without "." dot) to input stream wrapper
     */
    val unzippers = Map[String, InputStream => InputStream] (
      "gz" -> { new GZIPInputStream(_) },
      "bz2" -> { new BZip2CompressorInputStream(_, true) }
    )

    /**
     * use opener on file, wrap in un/zipper stream if necessary
     */
    private def open[T](file: FileLike[_], opener: FileLike[_] => T, wrappers: Map[String, T => T]): T = {
      val name = file.name
      val suffix = name.substring(name.lastIndexOf('.') + 1)
      wrappers.getOrElse(suffix, identity[T] _)(opener(file))
    }

    /**
     * open output stream, wrap in zipper stream if file suffix indicates compressed file.
     */
    def outputStream(file: FileLike[_]): OutputStream =
      open(file, _.outputStream(), zippers)

    /**
     * open input stream, wrap in unzipper stream if file suffix indicates compressed file.
     */
    def inputStream(file: FileLike[_]): InputStream =
      open(file, _.inputStream(), unzippers)

    /**
     * open output stream, wrap in zipper stream if file suffix indicates compressed file,
     * wrap in writer.
     */
    def writer(file: FileLike[_], charset: Charset = Codec.UTF8): Writer =
      new OutputStreamWriter(outputStream(file), charset)


    def reader(file: FileLike[_], charset: Charset = Codec.UTF8): Reader =
      new InputStreamReader(inputStream(file), charset)

    def readLines(file: FileLike[_])(proc: String => Unit): Unit = {
      val reader = this.reader(file)
      try {
        for (line <- reader) {
          proc(line)
        }
      }
      finally reader.close()
    }

  }

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
   * helper function to write line by line in a file
   * create file if doesn't exist
   * @param file name of the file as created by the createFile Function and saved in the HashMap
   * @param str string to be written in the file
   */
  private def logToFile(fileName: String, str: String)
  {
    if(!filesWriters.contains(fileName))
    {
      val file = new File(fileName)
      val outputStreamWriter = IOUtils.writer(file)
      val bufferedWriter:BufferedWriter = new BufferedWriter(outputStreamWriter)

      filesWriters += (fileName->bufferedWriter)
    }

    val writer = filesWriters(fileName)
    writer.write(str)
    writer.newLine()
  }
  /**
   * destructive function to flush and close all opened buffered writers
   */
  private def closeWriters ()
  {
    filesWriters.values.foreach(_.close)
  }


  def main(args: Array[String]) {
    //todo : add some requires here to check for arguments
    val startTime = System.nanoTime
    //arg1 = 0
    val option = args(0)

     /**
      * option master:
      * -----------
      *extracting language links related properties from the WikiData RDF Dumb File
      * and save them in a separated languagelinks.nt file
      */
    if(option == "master")
    {
      val outFileName = args(2)

      //languagelinks triples needed are those contain schema:about predicates and wikipediapages subjects which indicated wikipedia page
      val cond1 = "wikipedia.org/wiki"
      val cond2 = "<http://schema.org/about>"

      val inFile = new File(args(1))

      //iterating over dump files -- readlines accept arg of type File implicitly through RichFile.wrapFile
      var lineCounter = 0
      IOUtils.readLines(inFile){ln =>
        if (ln != null)
        {
          val triple = split(ln);

          //check if the triple is in the correct .ttl format
          if(triple.length ==4){

            if(ln.contains(cond1)&&ln.contains(cond2))
            {
              triple(0) = triple(0).replace(".wikipedia.org/wiki",".dbpedia.org/resource")
              val sub = UriDecoder.decode(triple(2))
              val obj = UriDecoder.decode(triple(0))
              logToFile(outFileName,sub+" "+"<http://www.w3.org/2002/07/owl#sameAs>"+" "+obj+" .")
            }
          }
        }
        if ((lineCounter % 100000) == 0)
        {
          println("finished reading " + lineCounter + "lines.." )
        }
        lineCounter += 1
      }

      println("finished reading " + lineCounter + "lines" )
      closeWriters
    }

    /**
     * option: specific:
     * ---------
     * from the extracted languagelinks.nt file
     * extracting language links and save them in languagelinks folder
     */
    if(option == "specific")
    {

      //creating folder for output files
      val baseDir = args(2)
      new File(baseDir).mkdir()
      //file extension  .ttl  .ttl.gz .ttl.bz2
      val outFileExtension = args(3)


      var Q = ""
      var oldQ = ""
      var triplesObjects = List[String]()

      val langRegx = """<http:\/\/(.*).dbpedia.*>""".r

      //iterating over LLmasterfile Triples  -- readLines accept arg of type File implicitly through RichFile.wrapFile
      val inFile = new File(args(1))

      IOUtils.readLines(inFile){ln =>

        //Q and old Q will keep it's state

        var tripleObj = ""
        if(ln != null)
        {
          val triple = split(ln);

          //gather all objects of triples until the subject changes
            oldQ = Q
            Q = triple(0)
            tripleObj = triple(2)
        }

        //for each chuck ( the subject changed or if the last line had passed) , make combinations and save to files

          if((oldQ != Q && oldQ != "") || ln == null)
          {
            //println(oldQ)
            for(obj <- triplesObjects)
            {
              //extracting language
              val langRegx(lang) = obj
              //initializing file name
              val outFileName = baseDir+"/interlanguage_links_same_as_"+lang+outFileExtension
              //removing itself
              val innerTripleObjects = triplesObjects.diff(List(obj))
              //logtofile function includes creating files if not exist
              for(obj2 <- innerTripleObjects)
              {
                logToFile(outFileName,obj +" <http://www.w3.org/2002/07/owl#sameAs> " +obj2+" .")
              }

            }

            //empty the Chunk container
            triplesObjects = List[String]()
          }

          if (ln != null)
          {
            triplesObjects = triplesObjects :+ tripleObj
          }

        }

      closeWriters()
    }

    print("time taken: " + (System.nanoTime - startTime)/1000000000 +" secs\n" )

  }


}

