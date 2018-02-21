package info.mukel.telegrambot4s.api

import akka.http.scaladsl.{Http, HttpsConnectionContext}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

trait WebRoutes extends BotBase with AkkaImplicits with BotExecutionContext {


  val port: Int
  val interfaceIp: String = "::0"

  def routes: Route = reject

  //Not force subclasses to implement this member, to ensure
  //backwards compatibility, and set it to null by default.
  protected val connectionContext: HttpsConnectionContext = null
  private var bindingFuture: Future[Http.ServerBinding] = _

  abstract override def run(): Unit = {
    super.run()
    bindingFuture =
      if (connectionContext == null)
        Http().bindAndHandle(routes, interfaceIp, port)
      else
        Http().bindAndHandle(routes, interfaceIp, port, connectionContext = connectionContext)

    bindingFuture.foreach { _ =>
      logger.info(s"Listening on $interfaceIp:$port")
    }

    sys.addShutdownHook {
      Await.ready(shutdown(), 30.seconds)
    }
  }

  abstract override def shutdown(): Future[Unit] = {
    super.shutdown().transformWith {
      _ =>
        for {
          b <- bindingFuture
          _ <- b.unbind()
          t <- system.terminate()
        } yield ()
    }
  }
}

