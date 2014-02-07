package com.td.utils

import scalaj.http.{HttpOptions, Http}
import scala.util.parsing.json.JSON._

class GoogleSearchAPI {
  private def HTTPGet(url: String) = {
    Http(url)
      .option(HttpOptions.connTimeout(5000))
      .option(HttpOptions.readTimeout(10000))
  }

  /**
   * Searches up images using some phrase
   * @param phrase
   * @return List of images
   */
  def SearchImage(phrase: String): Seq[String] = {
    val result = parseFull(HTTPGet("https://ajax.googleapis.com/ajax/services/search/images?v=1.0&safe=active&as_filetype=gif&imgtype=animated&q=" + phrase.replace(" ", "%20")).asString)
    result match {
      case Some(json) => json.asInstanceOf[Map[String, Any]].get("responseData") match {
        case Some(responseData) => responseData.asInstanceOf[Map[String, Any]].get("results") match {
          case Some(results) => results.asInstanceOf[List[Map[String, String]]].map( x=> x.get("url")).flatten
          case None =>Seq.empty
        }
        case None => Seq.empty
      }
      case None => Seq.empty
    }
  }
}

