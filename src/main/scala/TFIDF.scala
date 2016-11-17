import Main.Document

import scala.collection.immutable.Range.Inclusive

/**
 * Created by JG on 28/10/16.
 */
object TFIDF {

  def stem(document: Document):Document = {
    def stemLine(line:String):List[String]={
      line.toLowerCase.split(" ").map(stem).toList
    }
    val stemmedDocument:List[String] = stemLine(document.title) ::: document.content.map(stemLine).flatten ::: Nil
    document.copy(stemmed = stemmedDocument)
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

  def bagOfWords(keywords:List[String])(document: Document):List[Int] = {
    keywords.map(keyword=>document.stemmed.count(word=> word==keyword))
  }

  def bagOfWordsList(keywords:List[String])(document: List[String]):List[Int] = {
    keywords.map(keyword=>document.count(word=> word==keyword))
  }

  def toTF(bag:List[Int]):List[Double] = {
    val max=bag.max.toDouble
    bag.map(_.toDouble/max)
  }

  def forEachKeywordIn(stemmedKeywords:List[String]): Inclusive = {
    (0 to stemmedKeywords.size - 1)
  }

  def countOccurences(tfMatrix:List[List[Double]])(index:Int):Int = {
    tfMatrix.count(list => list(index) != 0.0)
  }

  def calculateIDF(tfMatrix:List[List[Double]])(noOfDocs:Int): Double = {
    if (noOfDocs == 0) 0 else Math.log(tfMatrix.size.toDouble / noOfDocs)
  }

  def cosineSimilarity(query:List[Double])(document:List[Double]):Double = {
    def vectorLength(query: List[Double]): Double = {
      Math.sqrt(query.map(Math.pow(_, 2)).sum)
    }

    val dotProduct=query.zip(document).map { case (tfValue: Double, idfValue: Double) => tfValue * idfValue }.sum
    val denominator=vectorLength(query)*vectorLength(document)
    if (denominator!=0 && !denominator.isNaN) dotProduct/denominator else 0
  }


}
