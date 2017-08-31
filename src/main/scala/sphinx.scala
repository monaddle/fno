/**
  * Created by danielporter on 8/15/17.
  */
class sphinx {

}


import java.io.File
import java.io.FileInputStream
import java.io.InputStream

import edu.cmu.sphinx.api.Configuration
import edu.cmu.sphinx.api.LiveSpeechRecognizer
import eitiab.main.MacLiveSpeechRecognizer


object TranscriberDemo {
  def main(args: Array[String]): Unit = {
    val configuration = new Configuration
    configuration.setAcousticModelPath("resource:/edu/cmu/sphinx/models/en-us/en-us")
    configuration.setDictionaryPath("resource:/edu/cmu/sphinx/models/en-us/cmudict-en-us.dict")
    configuration.setLanguageModelPath("resource:/edu/cmu/sphinx/models/en-us/en-us.lm.bin")
    val recognizer = new MacLiveSpeechRecognizer(configuration)
    // Start recognition process pruning previously cached data.
    recognizer.startRecognition(true)
    val result = recognizer.getResult
    // Pause recognition process. It can be resumed then with startRecognition(false).
    recognizer.stopRecognition()
  }
}