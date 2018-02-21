package info.mukel.telegrambot4s.api

import java.net.URI

import info.mukel.telegrambot4s.methods.ParseMode.ParseMode
import info.mukel.telegrambot4s.methods.ParseMode

import scala.annotation.implicitNotFound

object RichMessage {
  type RichMessage = List[MessageComponent]
  type MCApplier = (RichTextFormat[_ <: ParseMode], String) => String

  private lazy val boldApplier: MCApplier = (f, t) => f.bold(t)
  private lazy val italicApplier: MCApplier = (f, t) => f.italic(t)
  private lazy val codeLineApplier: MCApplier = (f, t) => f.inlineCode(t)
  private lazy val codeBlockApplier: MCApplier = (f, t) => f.blockCode(t)
  private lazy val plainApplier: MCApplier = (_, t) => t

  trait MessageComponent {
    val text: String
    val applier: MCApplier

    /**
      * Serializes to a string the current formatted components using the specified implicit format.
      * @param ev the format of the output string.
      * @tparam Mode the type of the format (Should be either ParseMode.Markdown or ParseMode.HTML)
      * @return the formatted string.
      */
    def mkMessage[Mode <: ParseMode](implicit ev: RichTextFormat[Mode]) = applier(ev, text)
  }

  case class BoldMessageComponent(override val text: String) extends MessageComponent {
    override val applier = boldApplier
  }

  case class ItalicMessageComponent(override val text: String) extends MessageComponent {
    override val applier = italicApplier
  }

  case class CodeLineMessageComponent(override val text: String) extends MessageComponent {
    override val applier = codeLineApplier
  }

  case class CodeBlockMessageComponent(override val text: String) extends MessageComponent {
    override val applier = codeBlockApplier
  }

  case class PlainMessageComponent(override val text: String) extends MessageComponent {
    override val applier = plainApplier
  }

  trait GenURLMessageComponent extends MessageComponent {
    val url: URI
    override lazy val applier: MCApplier = (f, t) => f.url(url, text)
  }
  case class URLMessageComponent(override val text: String, override val url: URI) extends GenURLMessageComponent
  case class MentionMessageComponent(override val text: String, mentionedUser: Int) extends GenURLMessageComponent {
    override lazy val url = new URI(s"tg://user?id=$mentionedUser")
  }

  /**
    * Format the input text as a bold message component.
    * @param text the text to be formatted.
    * @return the formatted text as a RichMessageComponent instance.
    */
  def fbold(text: String) = BoldMessageComponent(text)

  /**
    * Format the input text as a italic message component.
    * @param text the text to be formatted.
    * @return the formatted text as a RichMessageComponent instance.
    */
  def fitalic(text: String) = ItalicMessageComponent(text)

  /**
    * Format the input text as a url message component.
    * @param url the URI where the text redirects to once it is clicked.
    * @param text the text to be formatted.
    * @return the formatted text as a RichMessageComponent instance.
    */
  def furl(url: URI, text: String) = URLMessageComponent(text, url)

  /**
    * Format the input text as a mention message component.
    * @param user the user where the text redirects to once it is clicked.
    * @param text the text to be formatted.
    * @return the formatted text as a RichMessageComponent instance.
    */
  def fmention(user: Int, text: String) = MentionMessageComponent(text, user)

  /**
    * Format the input text as a single line code message component.
    * @param code the text to be formatted.
    * @return the formatted text as a RichMessageComponent instance.
    */
  def fcodeLine(code: String) = CodeLineMessageComponent(code)

  /**
    * Format the input text as a code block message component.
    * @param code the text to be formatted.
    * @return the formatted text as a RichMessageComponent instance.
    */
  def fcodeBlock(code: String) = CodeBlockMessageComponent(code)

  /**
    * Format the input text as a plain message component.
    * @param str the text to be formatted.
    * @return the formatted text as a RichMessageComponent instance.
    */
  implicit def fplain(str: String) = PlainMessageComponent(str)

  @implicitNotFound("An instance of RichTestFormat is required. Should be one of ")
  trait RichTextFormat[+T <: ParseMode] {
    def bold(component: String): String
    def italic(component: String): String
    def url(url: URI, text: String): String
    def mention(userId: Int, text: String): String = url(new URI(s"tg://user?id=$userId"), text)
    def inlineCode(code: String): String
    def blockCode(code: String): String
  }

  implicit object MarkdownTextFormat extends RichTextFormat[ParseMode.Markdown] {
    private lazy val clickableTextRegex = "[|]".r
    private lazy val inlineCodeRegex = "`|\n".r

    override def bold(component: String): String = s"*${component.replace("*", "\\*")}*"
    override def italic(component: String): String = s"_${component.replace("_", "\\_")}_"

    override def url(url: URI, text: String): String =
      /* Telegram seems not to be supporting escaping [, ], (, and ) from the urls? */
      s"[${clickableTextRegex.replaceAllIn(text, "")}](${url.toASCIIString})"
    override def inlineCode(code: String): String = s"`${inlineCodeRegex.replaceAllIn(code, "")}`"
    override def blockCode(code: String): String = s"```${code.replace("```", "")}```"
  }

  implicit object HTMLTextFormat extends RichTextFormat[ParseMode.HTML] {
    private lazy val escapingRegex = """(<|>|&)""".r
    private def escape(in: String): String = escapingRegex.replaceAllIn(in, x => x.group(1) match {
      case "<" => "&lt;"
      case ">" => "&gt;"
      case "&" => "&amp;"
    })

    override def bold(component: String): String = s"<b>${escape(component)}</b>"
    override def italic(component: String): String = s"<i>${escape(component)}</i>"
    override def url(url: URI, text: String): String = s"<a href=${'"'}${url.toASCIIString}${'"'}>${escape(text)}</a>"
    override def inlineCode(code: String): String = s"<code>${escape(code)}</code>"
    override def blockCode(code: String): String = s"<pre>${escape(code)}</pre>"
  }

  implicit class RichMessageSeq(seq: RichMessage) {
    /**
      * Serializes to a string the current formatted components using the specified format.
      * @param ev the format of the output string.
      * @tparam Mode the type of the format (Should be either ParseMode.Markdown or ParseMode.HTML)
      * @return the formatted string.
      */
    def mkMessage[Mode <: ParseMode](implicit ev: RichTextFormat[Mode]) = {
      val aproxBufSize = seq.foldLeft(0)((acc, b) => acc + b.text.length)
      val builder = new StringBuilder(aproxBufSize)
      seq.foreach(x => builder.append(x.applier(ev, x.text)))
      builder.toString()
    }
  }

  implicit class RichString(str: String) {
    /**
      * Format the current text as a bold message component.
      * @return the formatted text as a RichMessageComponent instance.
      */
    def bold = fbold(str)

    /**
      * Format the current text as a italic message component.
      * @return the formatted text as a RichMessageComponent instance.
      */
    def italic = fitalic(str)

    /**
      * Format the current text as a url message component.
      * @param url the URI where the text redirects to once it is clicked.
      * @return the formatted text as a RichMessageComponent instance.
      */
    def url(url: URI) = furl(url, str)

    /**
      * Format the current text as a mention message component.
      * @param user the user where the text redirects to once it is clicked.
      * @return the formatted text as a RichMessageComponent instance.
      */
    def mention(user: Int) = fmention(user, str)

    /**
      * Format the current text as a single line code message component.
      * @return the formatted text as a RichMessageComponent instance.
      */
    def codeLine = fcodeLine(str)

    /**
      * Format the current text as a code block message component.
      * @return the formatted text as a RichMessageComponent instance.
      */
    def codeBlock = fcodeBlock(str)

    /**
      * Format the current text as a plain message component.
      * @return the formatted text as a RichMessageComponent instance.
      */
    def plain = fplain(str)
  }
}