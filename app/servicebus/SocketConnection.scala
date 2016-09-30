package servicebus

import java.io.InputStreamReader
import java.net.{InetAddress, Socket}

import scala.io.BufferedSource

/**
  */
object SocketConnection {
  def connect(ip: String) {
    println("hey")
    val socket: Socket = new Socket(InetAddress.getByName(ip), 11000)
    println("wtf")
    val source: BufferedSource = new BufferedSource(socket.getInputStream)
    while (true) {
      val line: String = source.bufferedReader().readLine()
      println(line)
    }
  }

  def main(args: Array[String]): Unit = {
    SocketConnection.connect("51.4.231.251")
  }
}
