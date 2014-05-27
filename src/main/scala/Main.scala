import io.netty.bootstrap.ServerBootstrap
import io.netty.bootstrap.ServerChannelFactory

import io.netty.buffer.ByteBuf

import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel

import io.netty.handler.codec.http._

package nnsearch {

import collections.immutable._
import nnsearch.io.SpellCheckIO
import nnsearch.correction._
import nnsearch.serialization._
import nnsearch.search._

import scala.collection.mutable.ArrayBuffer

object Program {
  def processor(
    method: NearestSearch.Searcher,
    format: Serializer.Serializer) = 
  {
    (dictionary: Trie, toFind: String) => format(
      NearestSearch(dictionary, toFind, method) map {
	(pair: (String, Int)) => Corrector.correct(toFind, pair._1, pair._2)
      })
  }

  def usage() = println("sbt \"run [dictionary_file] [port_number]\"")

  def parameters(args: Array[String]) : Option[(String, Int)] = {
    if (args.length < 2) None
    else Some(((args(0), args(1).toInt)))
  }

  def run(dictionaryFile: String, port: Int) = {
    println("Loading dictionary ...")

    val dictionary = Trie(
      scala.io.Source.fromFile(dictionaryFile, "UTF-8").getLines)

    println("Dictionary loaded.")

    val bossGroup = new NioEventLoopGroup(1)
    val ioGroup = new NioEventLoopGroup(4)
    val bs = new ServerBootstrap()
      .group(bossGroup, ioGroup)
      .channelFactory(new ServerChannelFactory[NioServerSocketChannel]{
	override def newChannel(el: EventLoop, eg: EventLoopGroup) =
	  new NioServerSocketChannel(el, eg)
      })
      .childHandler(new ChannelInitializer[SocketChannel] {
	override def initChannel(ch: SocketChannel) {
	  ch.pipeline()
	  .addLast("httpRequestDecoder", new HttpRequestDecoder())
	  .addLast("httpResponseEncoder", new HttpResponseEncoder())
	  .addLast(ioGroup, "spellCorrector", SpellCheckIO(
	    (toFind, method, format) => processor(method, format)(
	      dictionary, toFind)
	  ))
	}
      })

    try {
      val future = bs.bind(port).sync()
      future.channel().closeFuture().sync()
    } catch {
      case ex: Any => println("error: " + ex.toString)
    }
  }

  def main(args: Array[String]): Unit = parameters(args) match {
    case Some((fileName, port)) => run(fileName, port)
    case _ => usage()
  }
}

} // !package nnsearch
