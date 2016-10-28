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
    querySearchEngine()
  }

  def querySearchEngine(): Unit = {
    println("Please enter your query and press ENTER:")
    val query=getExpandedQuery
    findResults(query).foreach(println)
  }


  def getExpandedQuery: String = {
    val originalQuery=StdIn.readLine
    val expansions=WordNet.synonyms(originalQuery)
    println("Please select the expansion")

    expansions.zipWithIndex.foreach{
      case(expansion, index) => println(index +": "+ expansion.mkString(" "))
    }
    println("Type expansion index")

    val selectedIndex=StdIn.readInt()
    expansions(selectedIndex).mkString(" ")
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
