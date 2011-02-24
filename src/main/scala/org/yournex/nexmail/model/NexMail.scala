package org.yournex.nexmail.model

/**
 * Created by IntelliJ IDEA.
 * User: apple
 * Date: 2/12/11
 * Time: 1:37 PM
 * To change this template use File | Settings | File Templates.
 */
import org.yournex.lib.mail._

class NexMail {
  var client : MailClient  = new MailClient
  var labeles = Map.empty[String,MailLabel]

  def username  = client.selectedUsername
  def server    = client.selectedServer
  def password  = client.selectedPassword

  def getLabels : Map[String,List[Int]] = {
    labeles = client.getLabels

    var ret_map = Map.empty[String,List[Int]]

    labeles foreach {
      case (name,mail) => ret_map += (name -> List(mail.getUnreadMessageCount(),mail.getMessageCount()))
    }
    ret_map
  }

  def getMessages(label:String, from:Int=1, to:Int=50):List[MailMessage] ={
    val lbl = client.getLabel(label)

    lbl.getMessages(from,to)

  }

  def login(username:String ,password:String, server:String, protocol:String): Boolean = {
    val url = format("%s://%s:%s@%s", protocol, username, password, server)
    println(url)
    client.init(url)

    return true
  }
}
