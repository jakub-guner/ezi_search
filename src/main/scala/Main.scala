import scala.collection.immutable.Range.Inclusive
import scala.io.{Source, StdIn}

/**
 * Created by JG on 20/10/16.
 */
object Main extends App{

  val keywordsFile = Source.fromFile(args(0)).getLines().toList
  val documentsFile = Source.fromFile(args(1)).getLines().toList

  val stemmedKeywords=keywordsFile.map(stem).distinct
  val bagOfWordsProcessor=bagOfWords(stemmedKeywords)_

  val documents=parseDocuments(documentsFile)

  val tf=documents
    .map(stem)
    .map(bagOfWordsProcessor)
    .map(toTF)

  val idf=forEachKeyword
    .map(countOccurences)
    .map(calculateIDF)

  val documentsTFIDF=tf
    .map(list => list.zip(idf))
    .map(list => list.map{case (tfValue:Double, idfValue:Double) => tfValue*idfValue})

  while(true){
    querySerchEngine()
  }

  def querySerchEngine(): Unit = {
    println("Please enter your query and press ENTER:")
    val query=StdIn.readLine
    findResults(query).foreach(println)
  }


  def findResults(query: String): List[(Double, String)] = {
    def getQueryTFIDF: List[Double] = {
      val bagOfWordsLst = bagOfWordsList(stemmedKeywords) _
      val stemmedQuery = query.toLowerCase.split(" ").map(stem)
      val bag: List[Int] = stemmedKeywords.map(keyword => stemmedQuery.count(word => word == keyword))
      val tf = toTF(bag)
      val query_tf_idf = tf.zip(idf).map { case (tfValue: Double, idfValue: Double) => tfValue * idfValue }
      query_tf_idf
    }

    val similarityToQuery=cosineSimilarity(getQueryTFIDF)_
    
    documentsTFIDF
      .map(similarityToQuery)
      .zip(documents)
      .filter({case(sim, doc)=>sim!=0})
      .map({case(sim, doc)=>(sim, doc.title)})
//      .foreach(println)

//    Nil
  }
  
  def cosineSimilarity(query:List[Double])(document:List[Double]):Double = {
    def vectorLength(query: List[Double]): Double = {
      Math.sqrt(query.map(Math.pow(_, 2)).sum)
    }
    
    val dotProduct=query.zip(document).map { case (tfValue: Double, idfValue: Double) => tfValue * idfValue }.sum
    val denominator=vectorLength(query)*vectorLength(document)
    if (denominator!=0 && !denominator.isNaN) dotProduct/denominator else 0

  }




  def bagOfWordsList(keywords:List[String])(document: List[String]):List[Int] = {
    keywords.map(keyword=>document.count(word=> word==keyword))
  }

  def forEachKeyword: Inclusive = {
    (0 to stemmedKeywords.size - 1)
  }

  def calculateIDF: (Int) => Double = {
    noOfDocs => Math.log(tf.size.toDouble / noOfDocs)
  }

  def countOccurences: (Int) => Int = {
    index => tf.count(list => list(index) != 0.0)
  }

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
