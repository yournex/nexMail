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
import javax.mail.event.{MessageCountListener, MessageCountEvent}
import javax.mail.internet.InternetAddress
import javax.mail.internet.MimeMessage
import javax.mail.internet.MimeMultipart
import javax.mail.internet.MimeBodyPart

import scala.util.control.Exception
import scala.util.matching.Regex

object Statics {
  val READ_ONLY  = javax.mail.Folder.READ_ONLY
  val READ_WRITE = javax.mail.Folder.READ_WRITE
  val NOTHING    = 0
}

class MailClient  {

  val supportedProtocol = List("IMAPS","IMAP")
  val defaultLabel      = "Inbox"

  var selectedProtocol: String = ""
  var selectedUsername: String = ""
  var selectedPassword: String = ""
  var selectedServer  : String = ""
  var selectedPort    : String = ""
  var selectedFolder  : String = ""
  var labels                   = Map.empty[String, MailLabel]
  var store: javax.mail.Store = null


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
      case _ => throw new ParseFailedException(url)
    }
  }

  def initialConnection() {
    var mail_prot ="imap"
    if(selectedProtocol == "IMAPS"){
      mail_prot = "imaps"
    }

      try {
        var props = System.getProperties()
        props.setProperty("mail.store.protocol", mail_prot)
        val session = Session.getDefaultInstance(props, null);
        store = session.getStore(mail_prot);
        store.connect(selectedServer, selectedUsername, selectedPassword)
      }catch {
        case e:javax.mail.AuthenticationFailedException => throw  new AuthenticationFailedException(selectedUsername,selectedServer)
        case e:javax.mail.MessagingException => e.getNextException match {
          case uh: java.net.UnknownHostException => throw  new  UnknownHostException(selectedServer)
          case e: java.net.ConnectException => throw new ConnectionRefusedException(selectedServer)
          case _ => throw e
        }


      }
  }

  def selectLabel(label: String) = {
    val inbox = store.getFolder(label);
    inbox.open(Statics.READ_ONLY);
    val messages = inbox.getMessages()
    "End"
  }

  def getLabel(labelName: String, labelStatus: Int=Statics.READ_ONLY  ) : MailLabel  ={
    if(labels.get(labelName) == None){
      getLabels
    }

    return  labels(labelName)
    //val fld: javax.mail.Folder = store.getFolder(labelName)
    //if(labelStatus != Statics.NOTHING)
    //  fld.open(labelStatus)
    //val  lbl = new MailLabel(fld)
    //return lbl

  }

  def getLabels() : Map[String, MailLabel] = {
    if(labels.size == 0 ) {
      val lbls : Array[javax.mail.Folder] = store.getDefaultFolder().list()
      for (lbl <- lbls){
        //TODO: Handle folders which containt folders
        if(lbl.getType == javax.mail.Folder.HOLDS_MESSAGES + javax.mail.Folder.HOLDS_FOLDERS)
          labels = labels + (lbl.getName -> new MailLabel(lbl))

      }
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
  var countAll = -1
  var countUnread = -1
  var name:String = folder.getFullName
  var messages = Map.empty[Int,MailMessage]

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


  def getMessages(start:Int,end:Int): List[MailMessage] ={
    open(Statics.READ_ONLY)
    //TODO:check index bounds
    var ret_msg:List[MailMessage] = List()

    var i = start
    for (msg <- folder.getMessages(start,end)  ){
        ret_msg :::= List(new MailMessage(msg))
        i+=1
    }

    ret_msg
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

  def getUnreadMessageCount(force:Boolean=false) :Int  = {
    if(force == true || countUnread == -1 )
      countUnread = folder.getUnreadMessageCount
    countUnread
  }

  def getFolderObj : javax.mail.Folder=  { folder }

  def getMessageCount(force:Boolean=false): Int = {
    //if( force == true || countAll == -1)
      countAll = folder.getMessageCount

    countAll
  }

  def close() = {
    //label.close()
  }

  def search () = {}
}

//not now
class MailMessage(in: javax.mail.Message) {
  val msg : javax.mail.Message =  in
  val mime :javax.mail.internet.MimeMessage = in.asInstanceOf[javax.mail.internet.MimeMessage]
  //override def toString(): String = { msg.getSubject }
  val ID = mime.getMessageID

  def getID = {
    ID
  }
  def getContent = {
    val body = mime.getContent
    var ret_str = ""

    body match {
      case st : String => ret_str = body.toString
      case mim : javax.mail.internet.MimeMultipart => val mim_mul =  body.asInstanceOf[javax.mail.internet.MimeMultipart]
        //import javax.mail.internet.MimeUtility

        //println(mim_mul.getCount)
        //val b2 = mim_mul.getBodyPart(mim_mul.getCount-2).asInstanceOf[javax.mail.internet.MimeBodyPart]
        //b2.setContentLanguage(Array("UTF-8"))
        for (i <- 0 to mim_mul.getCount-1 ){
          val part = mim_mul.getBodyPart(i)

          //  println(mim_mul.getBodyPart(mim_mul.getCount-1).getFileName)
          var disposition = part.getDisposition
          if(disposition !=null){

          }else {
              ret_str = mim_mul.getBodyPart(0).getContent.toString
          }
          //println(disposition)
        }
        //println(mim_mul.getBodyPart(mim_mul.getCount-1).getFileName)
        //ret_str = mim_mul.getBodyPart(mim_mul.getCount-2).getContent.toString



   //     println(mim_mul.getBodyPart(mim_mul.getCount-1).getContent)
    //    println(mim_mul.getCount)
    }
    //println(getSubject)
    //println( body )  //val k = body.asInstanceOf[javax.mail.internet.MimeMultipart]
    //println("******************")
    //println(getSubject)

    //try {
    //  println(body.asInstanceOf[javax.mail.internet.MimeMultipart].getBodyPart(1))
    //}catch {
    //  case _ => println("aaa")
    //}

    ret_str

  }

  def getFrom :List[MailAddress] = {
    var ret_lst:List[MailAddress] = List()

    for( frm <- msg.getFrom){
      ret_lst :::=  List(new MailAddress(frm))

    }
    ret_lst
  }

  def getReplyTo = {

    var ret_lst:List[MailAddress] = List()

    for( frm <- msg.getReplyTo){
      ret_lst :::=  List(new MailAddress(frm))

    }
    ret_lst
  }

  def getSubject = { msg.getSubject }

  def getMessageNumber = { msg.getMessageNumber }

  def getSentDate = { msg.getSentDate }

  def getReceivedDate = { msg.getReceivedDate}

  def getMessageObj : javax.mail.Message =  { msg }



  //TODO: implement below methods
  def getBody():String = {getContent}
  def hasAttached : Boolean = { false }
  def getTo() = {
    var ret_lst:List[MailAddress] = List()

    for( frm <- msg.getAllRecipients){
      ret_lst :::=  List(new MailAddress(frm))

    }


    ret_lst

  }

  def getCC() = {
    var ret_lst:List[MailAddress] = List()
    val allcc = mime.getHeader("Cc")
    if(allcc!=null){
      for( cc <- allcc){
        ret_lst :::=  List(new MailAddress(new InternetAddress(cc)))
      }
    }
    println(ret_lst)
  }
  def getBCC() = {}
  def getHeader(header:String) = {}
  def getHeaders() = {}
}

class MailAddress(in:javax.mail.Address){
  val addr = in
  val int_addr = in.asInstanceOf[InternetAddress]
  def getAddress = { int_addr.getAddress }
  def getPersonal = { if(int_addr.getPersonal==null) int_addr.getAddress else int_addr.getPersonal }
  override def toString = { "MailAddress : " + getAddress }
}

trait MailClientException extends Exception {

}

class UnknownHostException (host:String) extends MailClientException {
  val code = 404
  val hostname = host
  def getReason = { "Unknow Host: "+ host}
  override def toString() = { "org.yournex.lib.mail.UnknownHostException "+ getReason}

}

class AuthenticationFailedException(user:String,host:String) extends MailClientException{
  val code = 401
  val hostname = host
  val username = user
  def getReason = { "Authentication Failed for " + username + " on Host "+ host}
  override def toString() = { "org.yournex.lib.mail.AuthenticationFailedException "+ getReason}
}

//TODO: Mask password in error
class ParseFailedException(url:String) extends MailClientException {
  val code = 300
  val connectionUrl = url
  def getReason = { "Can't Parse conenction " + connectionUrl }
  override def toString() = { "org.yournex.lib.mail.ParseFailedException "+ getReason}

}

class ConnectionRefusedException(host:String) extends MailClientException {
  val code = 301
  val hostname = host
  def getReason = { "Connection refused for HOST :" + hostname }
  override def toString() = { "org.yournex.lib.mail.ConnectionRefusedException "+ getReason}

}
