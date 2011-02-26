package org.yournex.nexmail.model

/**
 * Created by IntelliJ IDEA.
 * User: apple
 * Date: 2/12/11
 * Time: 1:37 PM
 * To change this template use File | Settings | File Templates.
 */
import org.yournex.lib.mail._
import scala.util.control.Exception

class NexMail {
  var client : MailClient  = new MailClient
  var labeles = Map.empty[String,MailLabel]

  def getUsername  = client.getUsername
  //def server    = client.selectedServer
  //def password  = client.selectedPassword

  def getLabels : Map[String, MailLabel] = {
    labeles = client.getLabels
    labeles
  }

  def getMessages(label:String, from:Int=1, to:Int=50):List[MailMessage] ={
    val lbl = client.getLabel(label)
    if(lbl !=None){
      var chk_fr = from
      var chk_to = to
      if(chk_to > lbl.get.getMessageCount())
        chk_to = lbl.get.getMessageCount()
      if(chk_fr<0)
        chk_fr=0
      if(chk_fr > lbl.get.getMessageCount())
        chk_fr = lbl.get.getMessageCount()

      return lbl.get.getMessages(chk_fr,chk_to)
    }

    throw new Exception("TODO: handle with Exception")
  }

  def login(username:String ,password:String, server:String, protocol:String): Boolean = {
    val url = format("%s://%s:%s@%s", protocol, username, password, server)
    client.init(url)

    return true
  }
}

