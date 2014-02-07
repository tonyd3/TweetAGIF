
import com.td.utils.{GoogleSearchAPI, TwitterAPI}
import scalaj.http.Token


object Main {
  def main(args: Array[String]) {
    val tweetRegex = """(@[a-zA-Z0-9_]+) to (@[a-zA-Z0-9_]+) ([a-zA-Z0-9 .]+)""".r
    // Tweet a Gif Tokens
    val consumer = Token("", "")
    val accessToken = Token("", "")
    val twitterAPI = new TwitterAPI(consumer, accessToken)
    val googleSearchAPI = new GoogleSearchAPI()

    var id = 430438879590481920L
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
                          val status = googleSearchAPI.SearchImage(t.group(3)).collect {
                            case s: String => t.group(2) + " @" + tweet._3 + " " + s
                          }.filter { x => x.length < 140 }.head

                          println(status)
                          println(twitterAPI.Reply(tweet._1, status))
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