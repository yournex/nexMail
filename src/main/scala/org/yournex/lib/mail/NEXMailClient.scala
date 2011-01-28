package org.yournex.lib.mail

/**
 * User: Milad Rastian <milad inatsign yournex.org>
 * Date: 1/26/11
 * Time: 11:39 PM
 * MailClient is responsible for talking with Mail Server
 */

import java.util.Properties
import javax.mail.Folder
import javax.mail.Message
import javax.mail.MessagingException
import javax.mail.NoSuchProviderException
import javax.mail.Session
import javax.mail.Store

import scala.util.matching.Regex



class NEXMailClient (url: String) {
  init(url)
  val supportedProtocol = List("IMAPS")
  val defaultLabel      = "Inbox"

  var selectedProtocol: String = ""
  var selectedUsername: String = ""
  var selectedPassword: String = ""
  var selectedServer  : String = ""
  var selectedPort    : String = ""
  var selectedFolder  : String = ""
  var labels                   = Map.empty[String, NEXMailLabel]

  var store: javax.mail.Store = null

  //url : IMAPS://username:password@imap.domain.com:993
  def init(url: String) = {
    //parse url
    val mailURL = new Regex("""(""" + supportedProtocol.mkString("|") + """):\/\/([A-Za-z0-9\.-]+):([A-Za-z0-9\.-@]+)@([A-Za-z0-9-.]+)([:0-9]*)""")
    url match {
      case mailURL(selectedProtocol, selectedUsername, selectedPassword, selectedServer, selectedPort) => initialConnection()
      case _ => println("throw exepction")
    }
  }

  def initialConnection() {
    if(selectedProtocol == "IMAPS"){
      var props = System.getProperties()
      props.setProperty("mail.store.protocol", "imaps")
      val session = Session.getDefaultInstance(props, null);
      store = session.getStore("imaps");
      store.connect(selectedServer, selectedUsername, selectedPassword)
     }
  }

  def selectLabel(label: String) = {
    val inbox = store.getFolder(label);
    inbox.open(Folder.READ_ONLY);
    val messages = inbox.getMessages()
    "End"
  }

  def getLabels() ={
    val lbls : Array[javax.mail.Folder] = store.getDefaultFolder().list()
    for (lbl <- lbls){
      labels = labels + (lbl.getName -> new NEXMailLabel(lbl))
    }
  }

  def createLabel(label: String) :Boolean = {
   val inbox =  store.getFolder("Scala");
   inbox.create(1)

  }

}

class NEXMailLabel(in: javax.mail.Folder) {
  var label : javax.mail.Folder = in

  def name = label.getFullName

  def open = {
    if ( label.isOpen == false)
      label.open(Folder.READ_WRITE)
  }

  def create = {

  }

  def delete = {}

  def getMSG(index: Int) = {}

  def getMSGS(index: List[Int] ): List[NEXMailMSG] = {
    open

    var ret_msg: List[NEXMailMSG] = List()
    for ( msg <- label.getMessages) {
      ret_msg = ret_msg ::: List(new NEXMailMSG(msg))
    }

    ret_msg
  }

  def getNewMSGCount = {}

  def getUnreadMSGCount = {}

  def getFolder : javax.mail.Folder=  { label }

  def close = {}

  def serach () = {}
}

//not now
class NEXMailMSG(in: javax.mail.Message) {
  def msg : javax.mail.Message =  in

  def getFrom = {}

  def getReplyTo = {}

  def replay = {}

  def getSubject = {}

  def getMSGNum = {}

  def getSentDate = {}

  def getReceivedDate = {}

  def setFlag() = {}

  def search () = {}

  def getMessage : javax.mail.Message =  { msg }
}

class  NEXMailAddress {

}

