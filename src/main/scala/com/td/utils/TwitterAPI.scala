package com.td.utils

import scala.util.parsing.json.JSON._
import scala.util.parsing.json.{JSONArray, JSONObject}
import scalaj.http._
import scala.collection.immutable.HashMap
import java.lang
import twitter4j.{TwitterFactory, StatusUpdate}
import java.net.{HttpURLConnection, URL}
import twitter4j.conf.ConfigurationBuilder
import twitter4j.auth.AccessToken

class TwitterAPI(consumer: Token, accessToken: Token) {

  private def HTTPGet(url: String) = {
    Http(url)
      .option(HttpOptions.connTimeout(5000))
      .option(HttpOptions.readTimeout(10000))
  }

  private def getRateLimit = {
    parseRaw(
      HTTPGet("https://api.twitter.com/1.1/application/rate_limit_status.json")
        .oauth(consumer, accessToken)
        .asString
    )
  }

  def getUserLookupLimit: Integer = {
    getRateLimit match {
      case Some(json) =>
        json.asInstanceOf[JSONObject].obj.get("resources") match {
          case Some(resources) => resources.asInstanceOf[JSONObject].obj.get("users") match {
            case Some(users) => users.asInstanceOf[JSONObject].obj.get("/users/lookup") match {
              case Some(lookup) => lookup.asInstanceOf[JSONObject].obj.get("remaining") match {
                case Some(remaining) => remaining.asInstanceOf[Double].toInt
                case None => 0
              }
              case None => 0
            }
            case None => 0
          }
          case None => 0
        }
      case None => 0
    }
  }

  def getApplicationLimit: Integer = {
    getRateLimit match {
      case Some(json) =>
        json.asInstanceOf[JSONObject].obj.get("resources") match {
          case Some(resources) => resources.asInstanceOf[JSONObject].obj.get("application") match {
            case Some(users) => users.asInstanceOf[JSONObject].obj.get("/application/rate_limit_status") match {
              case Some(lookup) => lookup.asInstanceOf[JSONObject].obj.get("remaining") match {
                case Some(remaining) => remaining.asInstanceOf[Double].toInt
                case None => 0
              }
              case None => 0
            }
            case None => 0
          }
          case None => 0
        }
      case None => 0
    }
  }

  def getMentionsLimit: Integer = {
    getRateLimit match {
      case Some(json) =>
        json.asInstanceOf[JSONObject].obj.get("resources") match {
          case Some(resources) => resources.asInstanceOf[JSONObject].obj.get("statuses") match {
            case Some(statuses) => statuses.asInstanceOf[JSONObject].obj.get("/statuses/mentions_timeline") match {
              case Some(mentions_timeline) => mentions_timeline.asInstanceOf[JSONObject].obj.get("remaining") match {
                case Some(remaining) => remaining.asInstanceOf[Double].toInt
                case None => 0
              }
              case None => 0
            }
            case None => 0
          }
          case None => 0
        }
      case None => 0
    }
  }

  def getFollowersIdsLimit: Integer = {
    getRateLimit match {
      case Some(json) =>
        json.asInstanceOf[JSONObject].obj.get("resources") match {
          case Some(resources) => resources.asInstanceOf[JSONObject].obj.get("followers") match {
            case Some(users) => users.asInstanceOf[JSONObject].obj.get("/followers/ids") match {
              case Some(lookup) => lookup.asInstanceOf[JSONObject].obj.get("remaining") match {
                case Some(remaining) => remaining.asInstanceOf[Double].toInt
                case None => 0
              }
              case None => 0
            }
            case None => 0
          }
          case None => 0
        }
      case None => 0
    }
  }

  def getUsers(userId: String): Option[Any] = {
    parseFull(
      HTTPGet("https://api.twitter.com/1.1/users/lookup.json")
        .param("user_id", userId)
        .oauth(consumer, accessToken).asString
    )
  }


  /**
   * @param info
   * @return screen_name followers_count friends_count verified
   */
  def getUserInfo(info: HashMap[String, Any]): (String, Long, Long, Boolean) = (
    info.get("screen_name") match {
      case Some(screenName) => screenName.toString
      case None => ""
    },

    info.get("followers_count") match {
      case Some(followersCount) => followersCount.asInstanceOf[Double].toLong
      case None => 0
    },
    info.get("friends_count") match {
      case Some(friendsCount) => friendsCount.asInstanceOf[Double].toLong
      case None => 0
    },
    info.get("verified") match {
      case Some(verified) => verified.asInstanceOf[Boolean]
      case None => false
    }
    )

  /**
   *
   * @param since_id
   * @return Seq of (Tweet Ids, actual tweets, originating user screen_name)
   */
  def getUserMentions(since_id: Option[String]): Seq[(Long, String, String)] = {
    val response = since_id match {
      case Some(id) => parseFull(HTTPGet("https://api.twitter.com/1.1/statuses/mentions_timeline.json")
        .param("since_id", id)
        .param("stringify_ids", "true")
        .oauth(consumer, accessToken).asString)
      case None => parseFull(HTTPGet("https://api.twitter.com/1.1/statuses/mentions_timeline.json")
        .param("stringify_ids", "true")
        .oauth(consumer, accessToken).asString)
    }

    response match {
      case None => Seq.empty
      case Some(json) =>
        json.asInstanceOf[List[Map[String, Any]]].map {
          tweet =>
            (new lang.Double(tweet.get("id").get.toString).longValue(), tweet.get("text").get.toString, tweet.get("user").get.asInstanceOf[Map[String, String]].get("screen_name").get)
        }
    }
  }

  /**
   * Tweets out a String
   * @param status
   * @return
   */
  def tweet(status: String) = {
    Http.post("https://api.twitter.com/1.1/statuses/update.json")
      .param("status", status)
      .option(HttpOptions.connTimeout(5000))
      .option(HttpOptions.readTimeout(10000))
      .oauth(consumer, accessToken).asString
  }

  /**
   * Reply to a status with some String
   * @param in_reply_to_status_id
   * @param status
   * @return
   */
  def reply(in_reply_to_status_id: Long, status: String) = {
    Http.post("https://api.twitter.com/1.1/statuses/update.json")
      .param("status", status)
      .param("in_reply_to_status_id", in_reply_to_status_id.toString)
      .option(HttpOptions.connTimeout(5000))
      .option(HttpOptions.readTimeout(10000))
      .oauth(consumer, accessToken).asString
  }

  /**
   * This is just a temporary stop-gap until I can figure out how to fix scalaj-http for multipart+oauth
   */
  def replyWithMedia(in_reply_to_status_id: Long, message: String, mediaURL: String) = {
    val builder = new ConfigurationBuilder();
    builder.setOAuthConsumerKey(consumer.key);
    builder.setOAuthConsumerSecret(consumer.secret);
    val twitter = new TwitterFactory(builder.build()).getInstance(new AccessToken(accessToken.key, accessToken.secret));

    val status = new StatusUpdate(message);
    val connection = new URL(mediaURL).openConnection().asInstanceOf[HttpURLConnection]
    connection.setRequestMethod("GET")
    val in = connection.getInputStream
    status.setMedia("pic.gif", in)
    status.setInReplyToStatusId(in_reply_to_status_id)
    twitter.updateStatus(status)

    in.close()
    "Tweeted " + message + " With URL: " + mediaURL
  }
}
