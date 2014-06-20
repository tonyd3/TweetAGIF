
import com.td.utils.{GoogleSearchAPI, TwitterAPI}
import java.net.{HttpURLConnection, URL}
import scala.io.Source
import scalaj.http.Token


object Main {
  def main(args: Array[String]) {
    val tweetRegex = """(@[a-zA-Z0-9_]+) to (@[a-zA-Z0-9_]+) ([a-zA-Z0-9 .]+)""".r
    // Tweet a Gif Tokens
    val consumer = Token("", "")
    val accessToken = Token("", "")
    val twitterAPI = new TwitterAPI(consumer, accessToken)
    val googleSearchAPI = new GoogleSearchAPI()

    var id = args.headOption match {
      case Some(head) => head.toLong
      case None => 479814603560747010L //Magic # to start from...
    }

    while (true) {
      try {
        println(twitterAPI.getApplicationLimit)
        println(twitterAPI.getMentionsLimit)
        while (twitterAPI.getApplicationLimit == 0 && twitterAPI.getMentionsLimit == 0) {
          // Sleep while at API limit
          Thread.sleep(30000)
        }
        try {
          // reverse to get them in chronological order
          val tweets = twitterAPI.getUserMentions(Some(id.toString)).reverse
          tweets.foreach {
            tweet =>
              tweetRegex.findFirstMatchIn(tweet._2) match {
                case Some(t) =>
                  t.groupCount match {
                    case 3 => t.group(1).toLowerCase match {
                      case "@tweetagif" => {
                        if (id < tweet._1) {
                          println("In Response to " + tweet._1)

                          val urls = googleSearchAPI.SearchImage(t.group(3)).find {
                            mediaURL => {
                              val connection = new URL(mediaURL).openConnection().asInstanceOf[HttpURLConnection]
                              connection.setRequestMethod("GET")
                              val is = connection.getInputStream
                              val length = Stream.continually(is.read).takeWhile(-1 !=).map(_.toByte).toArray.length
                              is.close()
                              println(mediaURL + " " + length)
                              length < 3000000
                            }
                          }

                          urls.headOption match {
                            case Some(head) => {
                              val status = t.group(2) + " @" + tweet._3
                              println(twitterAPI.replyWithMedia(tweet._1, status, head))
                            }
                            case None => {
                              println("No GIFs found...")
                              twitterAPI.reply(tweet._1, "Sorry, no GIFs found...")
                            }

                          }
                          id = tweet._1 + 1;

                        }
                      }
                    }
                  }
                case None =>
              }
          }
          println(id)
          println("----------------")
          Thread.sleep(59000)
        }
        catch {
          case e: Exception => println("Exception!!! " + e); Thread.sleep(10000)
        }
      }

      catch {
        // This will catch exceptions for getting API limits, usually some connectivity issue
        // Just sleep for 10S and it should be fine.
        case e: Exception => println("Catch all..." + e); Thread.sleep(10000);
      }
    }
  }

}