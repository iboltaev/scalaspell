import io.netty.bootstrap.ServerBootstrap;
import io.netty.bootstrap.ServerChannelFactory;

import io.netty.buffer.ByteBuf;

import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel

import io.netty.handler.codec.http._

// nnsearch
package nnsearch {

import collections.immutable._
import nnsearch.io.SpellCheckIO
import nnsearch.correction._
import nnsearch.serialization.Serializer

import scala.collection.mutable.ArrayBuffer

object Program {
  def serializer(format: nnsearch.serialization.FormatEnum.Value) =
    if (format == nnsearch.serialization.FormatEnum.xml) 
      (seq: Seq[Corrector.Word]) => Serializer.toXml(seq).toString
    else (seq: Seq[Corrector.Word]) => Serializer.toJson(seq)

  def searcher(method: nnsearch.SearchMethod) = method match {
    case SearchMethod(SearchType.KNearest, k) => new NearestSearch.K(k)
    case SearchMethod(SearchType.DNearest, d) => new NearestSearch.Delta(d)
    case _ => new NearestSearch.K(1)
  }

  def processor(
    method: nnsearch.SearchMethod,
    format: nnsearch.serialization.FormatEnum.Value) = 
  {
    (dictionary: Trie, toFind: String) => serializer(format)(
      NearestSearch(dictionary, toFind, searcher(method)) map {
	(pair: (String, Int)) => Corrector.correct(toFind, pair._1)
      })
  }

  def main(args: Array[String]) {
    if (args.length < 1) throw new scala.RuntimeException("specify dictionary file");
    val dictionary = Trie(
      scala.io.Source.fromFile(args(0), "UTF-8").getLines)

    val bossGroup = new NioEventLoopGroup(1);
    val ioGroup = new NioEventLoopGroup(4);
    val bs = new ServerBootstrap()
      .group(bossGroup, ioGroup)
      .channelFactory(new ServerChannelFactory[NioServerSocketChannel]{
	override def newChannel(el: EventLoop, eg: EventLoopGroup) =
	  new NioServerSocketChannel(el, eg);
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
      val future = bs.bind(3535).sync()
      future.channel().closeFuture().sync()
    } catch {
      case ex: Any => println("error: " + ex.toString);
    }
  }
}

} // !package nnsearch
