package info.mukel.telegrambot4s

import info.mukel.telegrambot4s.methods.ParseMode
import info.mukel.telegrambot4s.methods.ParseMode.ParseMode

import scala.language.implicitConversions

/**
  * Useful/scary implicits to reduce boilerplate.
  *
  * Note that implicit Option conversion can have unexpected side effects,
  * use wisely; at your own risk.
  */
object Implicits {
  private val deprecationMessage = "This method is deprecated in favour of the RichMessage api that allows you to " +
    "build safe-escaped messages for both Markdown and HTML parse modes."
  implicit def toOption[T](v: T) : Option[T] = Option(v)

  implicit class MarkdownString(val s: String) extends AnyVal {
    @deprecated(message = deprecationMessage, since = "")
    def bold = s"*$s*"

    @deprecated(message = deprecationMessage, since = "")
    def italic = s"_${s}_"

    @deprecated(message = deprecationMessage, since = "")
    def urlWithAlt(alt: String) = s"[$alt]($s)"

    @deprecated(message = deprecationMessage, since = "")
    def altWithUrl(url: String) = s"[$s]($url)"

    @deprecated(message = deprecationMessage, since = "")
    def mention(userId: Int) = s"[$s]($$tg://user?id=$userId)"

    @deprecated(message = deprecationMessage, since = "")
    def inlineCode = s"`$s`"

    @deprecated(message = deprecationMessage, since = "")
    def blockCode(language: String = "text") = s"```$language\n$s\n```"
    // Markdown escape

    @deprecated(message = deprecationMessage, since = "")
    def md = s.replaceAll("([" + "*_`[".replaceAll("(.)", "\\\\$1") + "])", "\\\\$1")
  }
}
