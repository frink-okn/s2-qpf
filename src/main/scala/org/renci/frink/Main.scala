package org.renci.frink

import ox.*
import scribe.Level
import sttp.tapir.server.netty.sync.NettySyncServer

object Main extends OxApp.Simple:

  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(Level.Info))
    .replace()

  def run(using Ox): Unit =
    val port = sys.env.get("HTTP_PORT").flatMap(_.toIntOption).getOrElse(8080)
    val binding = useInScope(NettySyncServer().host("0.0.0.0").port(port).addEndpoints(Endpoints.all).start())(_.stop())
    println(s"Server started at http://localhost:${binding.port}. ")
    never
