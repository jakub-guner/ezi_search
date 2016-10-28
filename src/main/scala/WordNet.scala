import java.net.URL
import scala.collection.JavaConverters._

import edu.mit.jwi.Dictionary
import edu.mit.jwi.item.{IIndexWord, POS, IWordID, IWord}

/**
 * Created by JG on 28/10/16.
 */
//Uses JWI 2.4.0
//http://projects.csail.mit.edu/jwi/
object WordNet {

  val dict=new Dictionary(new URL("file", null , "./WordNet/dict/"))
  dict.open()

  def synonyms(word:String):List[List[String]] = {
    val indexedWord = dict.getIndexWord(word, POS.NOUN);

    def getAllSensesOfTheWord: List[IWordID] = {
      indexedWord.getWordIDs.asScala.toList
    }
    def getListOfSynonyms(sense: IWordID): List[String] = {
      dict.getWord(sense).getSynset.getWords.asScala.toList.map(_.getLemma)
    }

    for{
      sense:IWordID <- getAllSensesOfTheWord
    } yield getListOfSynonyms(sense)
  }

}
