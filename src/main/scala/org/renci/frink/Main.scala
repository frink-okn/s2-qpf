package org.renci.frink

import io.netty.handler.codec.http.HttpServerCodec
import org.renci.frink.kgf.Model
import org.renci.frink.kgf.Service
import ox.*
import scribe.Level
import sttp.tapir.server.netty.NettyConfig
import sttp.tapir.server.netty.sync.NettySyncServer

object Main extends OxApp.Simple:

  /** Longest request line accepted. Bindings are sent in the URL, and Comunica's 64 bindings per request are longer than Netty's default of
    * 4096 characters.
    */
  val MaxRequestLineLength = 1024 * 1024

  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(Level.Info))
    .replace()

  def run(using Ox): Unit =
    val port = sys.env.get("HTTP_PORT").flatMap(_.toIntOption).getOrElse(8080)
    warmUp()
    val server = NettySyncServer()
      .host("0.0.0.0")
      .port(port)
      .modifyConfig(
        _.initPipeline(config =>
          (pipeline, handler) =>
            NettyConfig.defaultInitPipeline(config)(pipeline, handler)
            pipeline.replace(classOf[HttpServerCodec], "serverCodecHandler", HttpServerCodec(MaxRequestLineLength, 8192, 8192))
        )
      )
    val binding = useInScope(server.addEndpoints(Endpoints.all).start())(_.stop())
    println(s"Server started at http://localhost:${binding.port}. ")
    never

  /** The fixed work every response needs, done before the port is bound rather than inside whichever request arrives first.
    *
    * Measured on this machine: the first request that needed an ETag cost 173 ms and the next 3 ms, because `Service.ContentDigest` hashes
    * the classpath the first time it is read (about 100 ms) and the description model's closed forms are computed the first time a response
    * reads them. Neither depends on the request, so neither belongs in one; a server that is listening is one that can answer.
    */
  private def warmUp(): Unit =
    Service.ContentDigest
    Model.Queryable.triples
