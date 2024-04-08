/*
 * Copyright (c)  2023. ONERA
 * This file is part of Dalculator
 *
 * Dalculator is free software ;
 * you can redistribute it and/or modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation ;
 * either version 2 of  the License, or (at your option) any later version.
 *
 * PML Analyzer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY ;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this program ;
 *  if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

package dalculator.utils

import java.io.{File, FileOutputStream, FileWriter}
import scala.io.{BufferedSource, Source}

object FileManager {

  /**
   * Util case class encoding an output directory used by exporters and solvers
   *
   * @param name the name of the directory stored at the project location
   */
  final case class OutputDirectory(name: String) {
    private val directory = {
      val file = new File(name)
      if (file.exists())
        file
      else if (file.mkdir()) {
        file
      } else {
        throw new Exception(s"cannot create $file directory")
      }
    }

    /**
     * Create a new file in the current directory
     *
     * @param s the file name
     * @return the java File
     */
    def getFile(s: String): File = {
      new File(directory, s)
    }

    /**
     * remove all the existing files of the directory
     */
    def clean(): Unit = {
      def deleteRecursively(f: File): Unit = {
        if (f.isDirectory) f.listFiles.foreach(deleteRecursively)
        f.delete()
      }

      deleteRecursively(directory)
    }

    /**
     * find recursively a file by its name
     *
     * @param name name of the file to find
     * @return the java File if found
     */
    def locate(name: String): Option[File] =
      OutputDirectory.recursiveLocateFirstFile(directory, (f: File) => f.getName == name)

  }

  object OutputDirectory {
    def recursiveLocateFirstFile(dir: File, filter: File => Boolean): Option[File] = {
      if (dir.exists()) {
        if (dir.isDirectory) {
          val these = dir.listFiles
          (for (r <- these.find(filter)) yield {
            r
          }) orElse {
            these.filter(_.isDirectory).flatMap(x => recursiveLocateFirstFile(x, filter)).toList match {
              case Nil => None
              case h :: _ => Some(h)
            }
          }
        } else {
          Some(dir).filter(filter)
        }
      } else
        None
    }
  }

  val analysisDirectory: OutputDirectory = OutputDirectory("analysis")

  val exportDirectory: OutputDirectory = OutputDirectory("export")

  val temporaryDirectory: OutputDirectory = OutputDirectory("tmp")

  val libraryDirectory: OutputDirectory = OutputDirectory("lib")

  val testDirectory: OutputDirectory = OutputDirectory("testTmp")

  def extractResourceAsTemporaryFile(resource: String): Option[String] =
    (for {
      p <- temporaryDirectory.locate(resource)
    } yield p.getAbsolutePath) orElse
      (
      for {
        input <- Option(getClass.getClassLoader.getResourceAsStream(resource))
      } yield {
        val file = temporaryDirectory.getFile(resource)
        val out = new FileOutputStream(file)
        val bytes = new Array[Byte](1024)
        Iterator
          .continually(input.read(bytes))
          .takeWhile(-1 != _)
          .foreach(x => out.write(bytes, 0, x))
        out.flush()
        out.close()
        file.getAbsolutePath
      })

  def extractResourceAsStream(name: String): Option[BufferedSource] = {
    val classLoader = getClass.getClassLoader
    val resource = Source.fromInputStream(classLoader.getResourceAsStream(name))
    Option(resource)
  }

  def extractResourceAsFile(name: String): Option[String] = {
    val classLoader = getClass.getClassLoader
    for {resource <- Option(classLoader.getResource(name))}
      yield resource.getFile
  }

  def locateFirstFileInEnv(envVarName: String, filter: File => Boolean): Option[File] = {
    val zero: Option[File] = None
    val env = System.getenv(envVarName)
    if (env != null) {
      env.split(System.getProperty("path.separator")).foldLeft(zero)((acc, p) => {
        acc match {
          case Some(_) => acc
          case None => locateFirstFile(new File(p), filter)
        }
      })
    } else {
      throw new Exception(s"environment variable $envVarName should be defined")
    }
  }

  private def locateFirstFile(dir: File, filter: File => Boolean): Option[File] = {
    if (dir.exists()) {
      if (dir.isDirectory) {
        val these = dir.listFiles
        for (r <- these.find(filter)) yield {
          r
        }
      } else {
        Some(dir).filter(filter)
      }
    } else {
      None
    }
  }
}
