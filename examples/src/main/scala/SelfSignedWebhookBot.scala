import java.io.File

import akka.http.scaladsl.{ConnectionContext, HttpsConnectionContext}
import info.mukel.telegrambot4s.api.{SSLContextProvider, TelegramBot, Webhook}
import info.mukel.telegrambot4s.methods.SendMessage
import info.mukel.telegrambot4s.models.{ChatId, InputFile, Message}

import scala.concurrent.duration.Duration
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * This bot uses a self signed certificate on the webhook, specified in the
  * bot specification. The keys that will be used are placed under the "resources" folder:
  *   - ss-private.key: The private key of the certificate (Keep it secret!)
  *   - ss-public.pem:  The public key of the certificate. This is the file which should be sent
  *                     to Telegram while setting the webhook.
  *   - server.p12:     A file containing both the private and the public key. This is the
  *                     file that the HTTPS server will use to secure the traffic.
  *
  *   This files may be generated with the following commands:
  *   <code>openssl req -newkey rsa:2048 -sha256 -nodes -keyout ss-private.key -x509 -days 365 -out ss-public.key</code>
  *   <code>openssl pkcs12 -export -out server.p12 -inkey ss-private.key -in ss-public.pem</code>
  *
  *   You must insert your own information while generating them, such as country, company name, server's <b>FQDN</b>...
  */
class SelfSignedWebhookBot(token: String) extends ExampleBot(token) with Webhook {
  override val webhookUrl: String = "https://66.66.66.66:8443"
  override val port: Int = 8443

  override def certificate: Option[InputFile] = Some(InputFile("ss-public.pem")) //Set the public PEM file
  override val maySetWebhookOnStart: Boolean = true //Set the webhook after starting the bot.
  override val webhookResetInterval: Duration = 5 days //Resend a setWebhook request each 5 days (set to Duration.Inf to disable)
  override protected val connectionContext: HttpsConnectionContext = ConnectionContext.https(
    SSLContextProvider.tlsFromPkcs12(
      new File("server.p12"),   /* The keystore of the key */
      "".toCharArray)           /* The password of the keystore (empty for this example) */
      .provide)

  override def receiveMessage(message: Message): Unit = {
    for (text <- message.text) {
      request(SendMessage(ChatId.Chat(message.chat.id), text, replyToMessageId = Some(message.messageId)))
    }
  }

  //Print a message when the webhook was attempted to be set.
  override def didUpdateWebhook(result: Try[Boolean]): Unit = result match {
    case Success(true) => println("Webhook set successfully")
    case Success(false) => println("Unknown error while setting webhook")
    case Failure(ex) => println("Error while setting webhook")
      ex.printStackTrace()
  }
}
