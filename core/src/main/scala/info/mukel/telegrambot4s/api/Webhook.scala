package info.mukel.telegrambot4s.api

import java.io.IOException

import akka.actor.Cancellable
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import info.mukel.telegrambot4s.marshalling.AkkaHttpMarshalling._
import info.mukel.telegrambot4s.methods.SetWebhook
import info.mukel.telegrambot4s.models.{InputFile, Update}

import scala.concurrent.Future
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/** Uses a webhook (as an alternative to polling) to receive updates.
  *
  * Automatically registers the webhook on run().
  */
trait Webhook extends WebRoutes {
  _ : BotBase with AkkaImplicits with BotExecutionContext =>

  private var resetWebhookTask: Option[Cancellable] = None

  /** URL for the webhook.
    *
    * 'webhookUrl' must be consistent with 'webhookRoute' (by default '/').
    */
  val webhookUrl: String

  /**
    * Webhook route.
    *
    * 'webhookUrl/' by default.
    *
    * @return Route handler to process updates.
    */
  def webhookRoute: Route = pathEndOrSingleSlash(webhookReceiver)

  def webhookReceiver: Route = {
    entity(as[Update]) { update =>
      try {
        receiveUpdate(update)
      } catch {
        case NonFatal(e) =>
          logger.error("Caught exception in update handler", e)
      }
      complete(StatusCodes.OK)
    }
  }

  /**
    * Certificate input file that will be sent to Telegram on a setWebhook
    * request.
    * @return An Optional InputFile. If set, the returned input file
    *         will be sent as certificate to Telegram. Otherwise,
    *         no certificate will be sent. Note that the last case
    *         will be only suitable in environments where the HTTPS certificate
    *         is signed by a trusted well-known CA.
    */
  def certificate: Option[InputFile] = None

  /**
    * Value that indicates whether the bot may send a setWebhook request
    * to Telegram after starting.
    */
  val maySetWebhookOnStart: Boolean = true /* Default value is true for backwards compatibility reasons */

  /**
    * If this value is a FiniteDuration the bot will automatically resend
    * a setWebhook request each time the specified interval has elapsed,
    * ensuring that the webhook is always set, even if it is inactive for
    * a long time. If the value is not a FiniteDuration, it will disable
    * this feature.
    */
  val webhookResetInterval: Duration = Duration.Inf

  abstract override def routes: Route =  webhookRoute ~ super.routes

  def didUpdateWebhook(result: Try[Boolean]): Unit = ()

  private def setWebhook: Future[Boolean] = {
    val f = request(SetWebhook(url = webhookUrl, certificate = certificate, allowedUpdates = allowedUpdates))
    f onComplete didUpdateWebhook
    f
  }

  abstract override def run(): Unit = {
    webhookResetInterval match {
      case fd: FiniteDuration =>
        resetWebhookTask = Some(system.scheduler.schedule(fd, fd, () => {
          setWebhook onComplete {
            case Success(false) =>
              throw new IOException("Scheduled webhook reset was unable to resend webhook request")
            case Failure(e) =>
              throw new IOException("Scheduled webhook reset was unable to resend webhook request", e)
            case _ =>
          }
        }))
      case _ =>
    }

    if (maySetWebhookOnStart) {
      val f = setWebhook
      f.onComplete {
        case Success(true) =>
          super.run() // Spawn WebRoute

        case Success(false) =>
          logger.error("Failed to set webhook")

        case Failure(e) =>
          logger.error("Failed to set webhook", e)
      }
    } else {
      super.run()
    }
  }

  abstract override def shutdown(): Future[Unit] = {
    resetWebhookTask match {
      case Some(x) => x.cancel()
      case _ =>
    }
    super.shutdown()
  }
}
