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
import javax.mail.internet.MimeMessage

import scala.util.matching.Regex

object Statics {
  val READ_ONLY  = javax.mail.Folder.READ_ONLY
  val READ_WRITE = javax.mail.Folder.READ_WRITE
  val NOTHING    = 0
}

class MailClient (url: String) {

  val supportedProtocol = List("IMAPS")
  val defaultLabel      = "Inbox"

  var selectedProtocol: String = ""
  var selectedUsername: String = ""
  var selectedPassword: String = ""
  var selectedServer  : String = ""
  var selectedPort    : String = ""
  var selectedFolder  : String = ""
  var labels                   = Map.empty[String, MailLabel]
  var store: javax.mail.Store = null

  init(url)

  def getStore : javax.mail.Store = { store }

  //url : IMAPS://username:password@imap.domain.com:993
  def init(url: String) = {
    //parse url
    val mailURL = new Regex("""(""" + supportedProtocol.mkString("|") + """):\/\/([A-Za-z0-9\.-]+):([A-Za-z0-9\.-@]+)@([A-Za-z0-9-.]+)([:0-9]*)""")
    url match {
      case mailURL(protocol, username, password, server, port) =>{
        selectedProtocol = protocol
        selectedUsername = username
        selectedPassword = password
        selectedServer   = server
        selectedPort     = port
        initialConnection()
      }
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
    inbox.open(Statics.READ_ONLY);
    val messages = inbox.getMessages()
    "End"
  }

  def getLabel(labelName: String, labelStatus: Int  ) : MailLabel  ={

    if(labels.contains(labelName)){
      if(labelStatus != Statics.NOTHING)
        labels(labelName.toString).open(labelStatus)
      return  labels(labelName.toString)
    }

    val fld: javax.mail.Folder = store.getFolder(labelName)
    if(labelStatus != Statics.NOTHING)
      fld.open(labelStatus)
    val  lbl = new MailLabel(fld)
    return lbl

  }

  def getLabels() : Map[String, MailLabel] ={
    val lbls : Array[javax.mail.Folder] = store.getDefaultFolder().list()
    for (lbl <- lbls){
      labels = labels + (lbl.getName -> new MailLabel(lbl))
    }
    labels
  }

  def createLabel(label: String) :Boolean = {
   val inbox =  store.getFolder("Scala");
   inbox.create(1)

  }

}

class MailLabel(in: javax.mail.Folder) {
  var folder : javax.mail.Folder = in

  def name:String = folder.getFullName

  def open (labelStatus:Int) = {
    if (!folder.isOpen){
      if(labelStatus != Statics.NOTHING)
        folder.open(labelStatus)
    }

  }

  def create = {
  }

  def delete = {}

  //TODO: return expaction if there's not message
  def getMessage(index: Int) : MailMessage = {
    open(Statics.READ_ONLY)
    val get_msg = folder.getMessage(index)

    return new MailMessage(get_msg)
  }

  def getMessages() : List[MailMessage] = {
    open(Statics.READ_ONLY)
    var ret_msg: List[MailMessage] = List()
    for ( msg <- folder.getMessages) {
      ret_msg = ret_msg ::: List(new MailMessage(msg))
    }

    ret_msg
  }

  def getNewMSGCount = {
    folder.getNewMessageCount
  }

  def getUnreadMSGCount :Int  = {
    folder.getUnreadMessageCount
  }

  def getFolderObj : javax.mail.Folder=  { folder }

  def close() = {
    //label.close()
  }

  def search () = {}
}

//not now
class MailMessage(in: javax.mail.Message) {
  def msg : javax.mail.Message =  in

  override def toString(): String = { msg.getSubject }

  def getFrom = { msg.getFrom map ( addr => addr.toString ) }

  def getReplyTo = { msg.getReplyTo map { addr => addr.toString } }

  def getSubject = { msg.getSubject }

  def getMessageNumber = { msg.getMessageNumber }

  def getSentDate = { msg.getSentDate }

  def getReceivedDate = { msg.getReceivedDate}

  def getMessageObj : javax.mail.Message =  { msg }



  //TODO: implement below methods
  def getBody():String = {"Body"}
  def hasAttached : Boolean = { false }
  def getTo() = {}
  def getCC() = {}
  def getBCC() = {}
  def getHeader(header:String) = {}
  def getHeaders() = {}
}
