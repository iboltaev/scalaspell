import io.netty.buffer.ByteBuf
import io.netty.buffer.Unpooled

import io.netty.channel._

import io.netty.handler.codec.http._

import io.netty.util.CharsetUtil

// nnsearch.io
package nnsearch.io {
import nnsearch._

import java.net.URLDecoder

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.TreeMap

/// quick & dirty url parsing without url validation
case class Url(path: String, vars: Map[String, String])

object Url {
  def apply(url: String, encoding: String = "en_US.UTF-8") = {
    def mapParams(iter: Iterator[String]) = iter
      .filter(! _.isEmpty)
      .map((v: String) => {
	val index = v indexOf '='
	if (index < 0) (v, "")
	else (v.take(index).trim,
	      URLDecoder.decode(v.drop(index + 1).trim, encoding))
      })
      .foldLeft(TreeMap[String, String]())(_ + _)

    val seq = url split Array('?', '&')
    if (url.contains('?') || seq.size == 1) 
      new Url(seq.head, mapParams(seq.tail.iterator))
    else new Url("", mapParams(seq.iterator))
  }
}

/// IO class, performs IO stuff
class SpellCheckIO(
  compute: (
    String, 
    search.NearestSearch.Searcher,
    serialization.Serializer.Serializer) => String)
extends ChannelHandlerAdapter 
{
  override def channelRead(ctx: ChannelHandlerContext,
			   msg: Any): Unit = msg match
  {
    case req: HttpRequest => {
      output(ctx, response(req))
    }
    case _ => { }
  }

  private def response(req: HttpRequest) = 
    if (req.getMethod() != HttpMethod.GET) { 
      val msg = "Unsupporded method"
      new DefaultFullHttpResponse(
	HttpVersion.HTTP_1_1,
	new HttpResponseStatus(400, msg), 
	Unpooled.copiedBuffer(msg, CharsetUtil.UTF_8))
    } else if (req.headers().contains("Expect", "100-Continue", true))
      new DefaultFullHttpResponse(
	HttpVersion.HTTP_1_1, HttpResponseStatus.CONTINUE)
    else {
      val request = Url(req.getUri)
      val toFind = request.vars.get("check").getOrElse("")
      val method = request.vars.get("find").getOrElse("")
      val format = request.vars.get("format").getOrElse("")

      val searcher = nnsearch.SearchParams.searcher(method)
      val serializer = nnsearch.SearchParams.serializer(format)

      val resp = 
	if (toFind.isEmpty) ""
	else compute(toFind, searcher, serializer)

      val r = new DefaultFullHttpResponse(
	HttpVersion.HTTP_1_1, 
	HttpResponseStatus.OK, 
	Unpooled.copiedBuffer(resp, CharsetUtil.UTF_8))

      r.headers().set("Content-Type", SearchParams.contentType(format))
      r.headers().set("Content-Length", resp.size.toString)

      r
    }

  private def output(ctx: ChannelHandlerContext, response: HttpResponse) =
  {
    ctx.write(response)
    ctx.flush()
    ctx.close()
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, t: Throwable) =
  {
    t.printStackTrace();
    ctx.close()
  }
}

object SpellCheckIO {
  def apply(
    compute: (String, 
	      nnsearch.search.NearestSearch.Searcher, 
	      nnsearch.serialization.Serializer.Serializer) => String) = 
    new SpellCheckIO(compute)
}

} // !nnsearch.io
