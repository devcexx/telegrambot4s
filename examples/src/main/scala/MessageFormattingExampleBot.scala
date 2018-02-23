import java.net.URI

import info.mukel.telegrambot4s.api.Polling
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.RichMessage._
import info.mukel.telegrambot4s.methods.ParseMode
import info.mukel.telegrambot4s.methods.SendMessage
import info.mukel.telegrambot4s.models.ChatId

/**
  * This bot is an example for sending formatted texts with the RichMessage api,
  * that allows you to concatenate message components safely escaped for, after,
  * converting them to a string with a recognizable format by Telegram (Markdown
  * or HTML).
  * @param token Bot's token.
  */
class MessageFormattingExampleBot(token: String) extends ExampleBot(token) with Polling with Commands {
  onCommand("/test") { msg =>
    val message1 = "Bold text; ".bold :: "Italic text; ".italic :: "Code; ".codeLine :: "Plain;".plain :: Nil
    val googleUrl = "Click ".plain :: "here".url(new URI("https://google.es?a=()")) :: " to navigate to ".plain :: "Google!".bold :: Nil
    val escapedMessage = "This text is ".plain :: "_safely_".bold :: "*escaped*".italic :: Nil
    val htmlMessage = "This message is ".plain :: "HTML-parsed".bold :: "<cats>And it's safe!</cats>".italic :: Nil
    val src = ChatId(msg.chat.id)
    for {
      _ <- request(SendMessage(src, message1.mkMessage[ParseMode.Markdown], parseMode = Some(ParseMode.Markdown)))
      _ <- request(SendMessage(src, googleUrl.mkMessage[ParseMode.Markdown], parseMode = Some(ParseMode.Markdown)))
      _ <- request(SendMessage(src, escapedMessage.mkMessage[ParseMode.Markdown], parseMode = Some(ParseMode.Markdown)))
      _ <- request(SendMessage(src, htmlMessage.mkMessage[ParseMode.HTML], parseMode = Some(ParseMode.HTML)))
    } yield ()
  }
}
