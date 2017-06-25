package org.scalalabs.intermediate.lab01

import java.util.Locale

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scala.xml._


trait TwitterStatus {
  val id: Long
  val inReplyToStatusId: Option[Long]
  val inReplyToUserId: Option[Long]
  val truncated: Boolean
  val favorited: Boolean
  val text: String
  val createdAt: DateTime
  val user: TwitterUser

}

object TwitterStatus {
  val dateFormatter = DateTimeFormat.forPattern("EE MMM dd HH:mm:ss Z yyyy").withLocale(Locale.US)

  def apply(node: Node) = {
    new TwitterStatus {
      val id = (node \ "id").text.toLong
      val inReplyToStatusId =
        if ((node \ "in_reply_to_status_id").text != "")
          Some((node \ "in_reply_to_status_id").text.toLong)
        else
          None

      val inReplyToUserId =
        if ((node \ "in_reply_to_user_id").text != "")
          Some((node \ "in_reply_to_user_id").text.toLong)
      else
          None
      val truncated = (node \ "truncated").text.toBoolean
      val favorited = (node \ "favorited").text.toBoolean
      val text = (node \ "text").text
      val createdAt = dateFormatter.parseDateTime((node \ "created_at").text)
      val user = TwitterUser((node \ "user") (0))

    }
  }
}
