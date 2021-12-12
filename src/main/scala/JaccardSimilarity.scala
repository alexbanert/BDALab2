import scala.io.{BufferedSource, Source}
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer

object JaccardSimilarity {

  val stopwords: List[String] = getStopwords()

  def filterBook(f: BufferedSource): Array[String] = {
    val book = f.mkString.toLowerCase.replaceAll("[-/’“\",.!:?*;»…()«#”]", "").split("\\s+")
    book.filterNot(stopwords.contains(_))
  }

  def shingles(text: Array[String], k: Int): Set[String] = {
    return text.flatMap(word => word.toSeq.sliding(k).map(_.unwrap).toSet).toSet
  }

  def jaccard(text1: Array[String], text2: Array[String], k: Int): Double = {
    val text1Shingles = shingles(text1, k)
    val text2Shingles = shingles(text2, k)
    jaccardSimSets(text1Shingles, text2Shingles)
  }

  def jaccardSimSets(text1: Set[String], text2: Set[String]) = {
    text1.intersect(text2).size.asInstanceOf[Double] / text1.union(text2).size
  }

  def main(args: Array[String]): Unit = {
    val book1 = Source.fromFile("src/main/scala/resources/lotr1.txt", "UTF-8")
    val book2 = Source.fromFile("src/main/scala/resources/lotr2.txt", "UTF-8")
    val book3 = Source.fromFile("src/main/scala/resources/lotr3.txt", "UTF-8")
    val book4 = Source.fromFile("src/main/scala/resources/kafka.txt", "UTF-8")
    val book5 = Source.fromFile("src/main/scala/resources/lordflies.txt", "UTF-8")
    val book6 = Source.fromFile("src/main/scala/resources/catcher.txt", "UTF-8")


    val books_list = Seq(book1, book2, book3, book4, book5, book6)
    var books = ListBuffer.empty[Array[String]]
    for (book <- books_list) {
      books += filterBook(book)
    }

    for (shingleSize <- 4 until 14) {
      println(s"Shingle size: ${shingleSize}")
      for (bookAindex <- 0 until books.length) {
        for (bookBindex <- (bookAindex + 1) until books.length) {
          println(s"Similarity of Book${bookAindex + 1} and Book${bookBindex + 1}:         " + jaccard(books(bookAindex), books(bookBindex), shingleSize).toString)
        }
      }
    }
  }

  def getStopwords() = {
    val stopWordsFilepath = "src/main/scala/resources/stopwords.txt"
    val sourceFile = Source.fromFile(stopWordsFilepath)
    val stopWords = try sourceFile.getLines().toList finally sourceFile.close()
    stopWords
  }
}
