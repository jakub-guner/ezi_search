import scala.collection.immutable.Range.Inclusive
import scala.io.{Source, StdIn}
import TFIDF._

/**
 * Created by JG on 20/10/16.
 */
object Main extends App{

  val keywordsFile = Source.fromFile(args(0)).getLines().toList
  val documentsFile = Source.fromFile(args(1)).getLines().toList

  val stemmedKeywords=keywordsFile.map(stem).distinct
  val bagOfWordsProcessor=bagOfWords(stemmedKeywords)_

  case class Document(title:String, content:List[String], stemmed:List[String] = List.empty[String])

  val documents=parseDocuments(documentsFile)

  val tf=documents
    .map(stem)
    .map(bagOfWordsProcessor)
    .map(toTF)

  val idf=forEachKeywordIn(stemmedKeywords)
    .map(countOccurences(tf)(_))
    .map(calculateIDF(tf)(_))

  val documentsTFIDF=tf
    .map(list => list.zip(idf))
    .map(list => list.map{case (tfValue:Double, idfValue:Double) => tfValue*idfValue})

  while(true){
    try{
      querySearchEngine()
    }catch{
      case e:Exception => println("Error occured. Try something different")
    }
  }

  def querySearchEngine(): Unit = {
    println("Please enter your query and press ENTER:")
    val (query, originalSize, weight)=getQueryFromUser
    findResults(query, originalSize, weight).foreach(println)
  }


  def getQueryFromUser: (String, Int, Double) = {
    val originalQuery=StdIn.readLine
    val originalQuerySize=originalQuery.split(" ").size
    println("Do you wish do expand your query y/n:")
    if(StdIn.readLine()=="y"){
      println("Enter the weight for expansion terms [0, 1]")
      val weight=StdIn.readDouble()
      (getExpandedQuery(originalQuery),originalQuerySize, weight)
    }else{
      println("Using original query...")
      (originalQuery,originalQuerySize, 0)
    }
  }

  def getExpandedQuery(originalQuery: String): String = {
    val expandedQuery =
      originalQuery
        .split(" ")
        .map(expandSingleWord)
        .mkString(" ")

    val finalQuery = originalQuery + " " + expandedQuery
    println(s"Searching for '$finalQuery'")
    finalQuery
  }

  def expandSingleWord(word:String):String={
    println(s"Please select the expansion for '$word'")
    val expansions=WordNet.synonyms(word)

    expansions
      .zipWithIndex
      .foreach{
      case(expansion, index) => println(index +": "+ expansion.mkString(" "))
    }
    println("Type index of expansion or -1 for empty one")

    val selectedIndex=StdIn.readInt()
    if(selectedIndex < 0) ""
    else expansions(selectedIndex).mkString(" ")

  }

  def findResults(query: String): List[(Double, String)] = {
    findResults(query, query.split(" ").size, 0.0)
  }

  def findResults(query: String, originalQueryLength:Int, weight:Double): List[(Double, String)] = {
    def getQueryTFIDF: List[Double] = {
      val bagOfWordsLst = bagOfWordsList(stemmedKeywords) _
      val stemmedQuery = query.toLowerCase.split(" ").map(stem)
      val expansion=stemmedQuery.drop(originalQueryLength)
      val bag: List[Int] = stemmedKeywords.map(keyword => stemmedQuery.count(word => word == keyword))
      val tf = toTF(bag).zipWithIndex.map{
        case (tf, index)=> if(expansion.contains(stemmedKeywords(index))) tf*weight else tf
      }
      val query_tf_idf = tf.zip(idf).map { case (tfValue: Double, idfValue: Double) => tfValue * idfValue }
      query_tf_idf
    }

    val similarityToQuery=cosineSimilarity(getQueryTFIDF)_
    
    documentsTFIDF
      .map(similarityToQuery)
      .zip(documents)
      .filter({case(sim, doc)=>sim!=0})
      .map({case(sim, doc)=>(sim, doc.title)})

  }

  def parseDocuments(documents:List[String]):List[Document]={
    def removeFirstLine(others: List[String]): List[String] = {
      others.drop(1)
    }

    if(documents.nonEmpty){
      val (firstDocument, otherDocuments) = documents.span(_.nonEmpty)
      val (titleList, contentList) = firstDocument.splitAt(1)

      val d = Document(titleList.head, contentList)

      d :: parseDocuments(removeFirstLine(otherDocuments))
    }else Nil
  }





}
