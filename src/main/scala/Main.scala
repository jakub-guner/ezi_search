import scala.io.Source

/**
 * Created by JG on 20/10/16.
 */
object Main extends App{

  val keywords = Source.fromFile(args(0)).getLines().toList
  val documents = Source.fromFile(args(1)).getLines().toList

  val stemmedKeywords=keywords.map(stem).toList
  val bagOfWordsProcessor=bagOfWords(stemmedKeywords)_

  val tf=parseDocuments(documents)
    .map(stem)
    .map(bagOfWordsProcessor)
    .map(toTF)
    .foreach(println)




  case class Document(title:String, content:List[String], stemmed:List[String] = List.empty[String])

  def toTF(bag:List[Int]):List[Double] = {
    val max=bag.max.toDouble
    bag.map(_.toDouble).map(_/max)
  }

  def stem(document: Document):Document = {
    def stemLine(line:String):List[String]={
      line.toLowerCase.split(" ").map(stem).toList
    }
    val stemmedDocument:List[String] = stemLine(document.title) ::: document.content.map(stemLine).flatten ::: Nil
    document.copy(stemmed = stemmedDocument)
  }

  def bagOfWords(keywords:List[String])(document: Document):List[Int] = {
    keywords.map(keyword=>document.stemmed.count(word=> word==keyword))
  }

  def parseDocuments(documents:List[String]):List[Document]={
    if(documents.nonEmpty){
      val (firstDocument, otherDocuments) = documents.span(_.nonEmpty)
      val (titleList, contentList) = firstDocument.splitAt(1)

      val d = Document(titleList.head, contentList)

      d :: parseDocuments(removeFirstLine(otherDocuments))
    }else Nil
  }


  def removeFirstLine(others: List[String]): List[String] = {
    others.drop(1)
  }

  def stem(word: String) : String = {
    val stemmer = new Stemmer()
    stemmer.add(word.trim)
    if (stemmer.b.length > 2) {
      stemmer.step1()
      stemmer.step2()
      stemmer.step3()
      stemmer.step4()
      stemmer.step5a()
      stemmer.step5b()
    }
    stemmer.b
  }



}
