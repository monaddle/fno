package eitiab.main

import edu.cmu.sphinx.api._
import edu.cmu.sphinx.frontend.util.StreamDataSource
import edu.cmu.sphinx.recognizer.Recognizer
import edu.cmu.sphinx.api.Context
import javax.sound.sampled.{AudioFormat, AudioSystem, Mixer}

/**
  * Created by danielporter on 8/15/17.
  */
case class MacLiveSpeechRecognizer(configuration: Configuration) extends AbstractSpeechRecognizer(configuration: Configuration) {
  println(AudioSystem.getMixerInfo())

  import javax.sound.sampled.AudioFormat
  import javax.sound.sampled.AudioSystem
  import javax.sound.sampled.TargetDataLine
  import javax.sound.sampled.AudioSystem
  import javax.sound.sampled.Mixer

  import javax.sound.sampled.AudioSystem
  import javax.sound.sampled.Line
  import javax.sound.sampled.LineUnavailableException
  import javax.sound.sampled.Mixer
  import java.util

  val mixers: Array[Mixer.Info] = AudioSystem.getMixerInfo
  val availableLines = new util.ArrayList[Line.Info]
  for (mixerInfo <- mixers) {
    System.out.println("Found Mixer: " + mixerInfo)
    val m = AudioSystem.getMixer(mixerInfo)
    val lines = m.getTargetLineInfo
    for (li <- lines) {
      System.out.println("Found target line: " + li)
      try {
        m.open()
        availableLines.add(li)
      } catch {
        case e: LineUnavailableException =>
          System.out.println("Line unavailable.")
      }
    }
  }

  System.out.println("Available lines: " + availableLines)

  import javax.sound.sampled.AudioSystem
  import javax.sound.sampled.Line
  import javax.sound.sampled.TargetDataLine

  val portInfo = new Line.Info(classOf[Nothing])
  var lineInfoAr: Array[Line.Info] = AudioSystem.getSourceLineInfo(portInfo)
  lineInfoAr = AudioSystem.getTargetLineInfo(portInfo)
  val tdlInfo = new Line.Info(classOf[TargetDataLine])
  lineInfoAr = AudioSystem.getSourceLineInfo(tdlInfo)
  lineInfoAr = AudioSystem.getTargetLineInfo(tdlInfo)
  val format = new AudioFormat(8000.0f, 16, 1, true, true)
  val microphone2: TargetDataLine = AudioSystem.getTargetDataLine(format)
  final private var microphone: Microphone = _
  microphone = new Microphone(8000.0f, 16,true, false)
  context.getInstance(classOf[StreamDataSource])
    .setInputStream(microphone.getStream())



  import javax.sound.sampled.DataLine
  import javax.sound.sampled.TargetDataLine
  val line = null

  def startRecognition(clear: Boolean): Unit = {
    recognizer.allocate()
    microphone.startRecording()

  }
  def stopRecognition(): Unit = {
    microphone.stopRecording()
    recognizer.deallocate()
  }
}

