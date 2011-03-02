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
/** MailClient is responsible for handling connection to IMAP server */
class MailClient  {
  /** List of protocol which it supports */
  private val supportedProtocol = List("IMAPS","IMAP")
  /** Default label that select */
  private val defaultLabel      = "Inbox"
  /** Selected protocol which currently is used*/
  private var selectedProtocol: String = ""
  /** Selected username which currently is used*/
  private var selectedUsername: String = ""
  /** Selected password which currently is used*/
  private var selectedPassword: String = ""
  /** Selected server which currently is used*/
  private var selectedServer  : String = ""
  /** Selected port which currently is used*/
  private var selectedPort    : String = ""
  /** Selected folder which currently is used*/
  private var selectedFolder  : String = ""
  /** List of all labels */
  private var labels                   = Map.empty[String, MailLabel]
  /** Store obj from javax.mail */
  private var store: javax.mail.Store = null

  /**get connected username
   * @return String username
   */
  def getUsername =  selectedUsername

  /**get connected Store Object
   * @return javax.mail.Store store Object
   */
  def getStore : javax.mail.Store = { store }

  /** Parse connection URL
   * @param url String format like IMAPS://username:password@imap.domain.com:993
   * @throws ParseFailedException
   */
  def init(url: String) = {
    //parse url
    val mailURL = new Regex("""(""" + supportedProtocol.mkString("|") + """):\/\/([A-Za-z0-9\.-@]+):([A-Za-z0-9\.-@]+)@([A-Za-z0-9-.]+)([:0-9]*)""")
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

  /** Initial connection to given data
   * @throws AuthenticationFailedException
   * @throws UnknownHostException
   * @throws ConnectionRefusedException
   * @throws javax.mail.MessagingException as unkown exception
   */
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

  /**
   * Select a lable object
   * @param label Name of label
   * @return Some[MailLabel] if label exists or None
   */
  def selectLabel(label: String):Option[MailLabel] = {
    labels.get(label)
  }

  /**
   * Return label Object
   * @param label Name of label
   * @param labelStatus Status of label
   * @return Object of label if exists
   */
  def getLabel(labelName: String, labelStatus: Int=Statics.READ_ONLY  ) : Option[MailLabel]  ={
    if(labels.get(labelName) == None){
      getLabels
    }

    return  labels.get(labelName)
  }

  /**
   * Return all labels as Map
   * @return Object of labels
   */
  def getLabels() : Map[String, MailLabel] = {
    if(0 == 0 ) {
      val lbls : Array[javax.mail.Folder] = store.getDefaultFolder().list()
      for (lbl <- lbls){

        labels = labels + (lbl.getName -> new MailLabel(lbl))
      }
    }
    labels
  }

  /**
   * Create label (it's not implemented yet)
   * @return true if label is created
   * @throws TODO
   */
  def createLabel(label: String) :Boolean = {
   //val inbox =  store.getFolder("Scala");
   //inbox.create(1)
   false
  }

}

/**
 * MailLabel handle access to labels
 * @param in Main java mail class that this object works with it
 */
class MailLabel(in: javax.mail.Folder) {
  /** Main java mail class that this object works with it */
  private var folder : javax.mail.Folder = in
  /** number of All Messages in this label*/
  private var countAll = -1
  /** number of All Unread Messages in this label */
  private var countUnread = -1
  /** Name of this label */
  var name:String = folder.getFullName
  /** Mesages in this label */
  private var messages = Map.empty[Int,MailMessage]
  /** Contain labels which is in this label */
  private val subLabel = for(fld <- folder.list.toList) yield new MailLabel(fld)

  /**
   * Return Sub labels in this label
   * @return Labels in this label as Map
   */
  def getSubLabels : Map[String,MailLabel] = {
    var ret =  Map.empty[String, MailLabel]
    subLabel foreach {
      lbl => ret += (lbl.name -> lbl)
    }
    ret
  }

  /**
   * Open this label
   * @param Status of Opening
   */
  def open (labelStatus:Int) = {
    if (!folder.isOpen){
      if(labelStatus != Statics.NOTHING)
        folder.open(labelStatus)
    }

  }

  /**
   * Create a this label (it's not implemented yet)
   */
  def create = {
  }

  /*
   * Remove this label (it's not implemented yet)
   */
  def remove = {}


  /**
   * Get Message with given number
   * @param index Index of message
   * @return Object of message
   */
  def getMessage(index: Int) : Option[MailMessage] = {
    open(Statics.READ_ONLY)
    val get_msg = folder.getMessage(index)
    //TODO: return None if there's not message with that index
    return Some(new MailMessage(get_msg))
  }

  /**
   * Get Messages with start index to end index
   * @param start Start index
   * @param end End index
   * @return List of Message object
   */
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

  /**
   * Get All Messages in this label
   * @return List of Messages objects
   */
  def getMessages() : List[MailMessage] = {
    open(Statics.READ_ONLY)
    var ret_msg: List[MailMessage] = List()
    for ( msg <- folder.getMessages) {
      ret_msg = ret_msg ::: List(new MailMessage(msg))
    }

    ret_msg
  }

  /**
   * Get number of unread Messages
   * @return Number of unread Messages
   */
  def getUnreadMessageCount(force:Boolean=false) :Int  = {
    //if this folder just can holds folder return -1
    if(folder.getType == javax.mail.Folder.HOLDS_FOLDERS)
      return -1
    if(force == true || countUnread == -1 )
      countUnread = folder.getUnreadMessageCount
    countUnread
  }

  /**
   * Get folder object
   */
  def getFolderObj : javax.mail.Folder=  { folder }

  /**
   * Get number of Messages
   * @return Number of Messages
   */
  def getMessageCount(force:Boolean=false): Int = {
    //if this folder just can holds folder return -1
    if(folder.getType == javax.mail.Folder.HOLDS_FOLDERS)
      return -1

    if( force == true || countAll == -1)
      countAll = folder.getMessageCount

    countAll
  }

  /**
   * Close this label (it's not implemented yet)
   */
  def close() = {
    //label.close()
  }

  /**
   * Search in this label (it's not implemented yet)
   */
  def search () = {}
}


/**
 * MailMessage handle access to message
 * @param in Main java mail class that this object works with it
 */
class MailMessage(in: javax.mail.Message) {
  /** Main java mail class that this object works with it*/
  private val msg : javax.mail.Message =  in
  /** Mime class from java mail*/
  private val mime :javax.mail.internet.MimeMessage = in.asInstanceOf[javax.mail.internet.MimeMessage]
  /** Boolean that saved file attached*/
  private var hasAttached : Boolean =  false
  /** Name of files which is attached in this message*/
  private var attachedFilesName :List[String] = List()
  /** ID of this message */
  private val ID = mime.getMessageID

  /**
   * get ID of this message
   * @return ID of this message
   */
  def getID = {
    ID
  }


  private def getContent = {
    val body = mime.getContent
    var ret_str = ""

    body match {
      case st : String =>  ret_str = body.toString
      case mim : javax.mail.internet.MimeMultipart =>
        ret_str = getText(body.asInstanceOf[javax.mail.internet.MimeMultipart])
    }

    ret_str
  }

  private def getText(multiMess:javax.mail.internet.MimeMultipart, result:String="html"):String = {
    var text = ""
    var html =  ""
    val match_html = """html""".r
    for(i <- 0 to (multiMess.getCount-1) ){
      val part = multiMess.getBodyPart(i)
      if(part.getDisposition == javax.mail.Part.ATTACHMENT){
        hasAttached = true
        attachedFilesName :::= List(multiMess.getBodyPart(i).getFileName)
        //TODO: parse com.sun.mail.util.BASE64DecoderStream
      }else if (part.getDisposition == javax.mail.Part.INLINE){
            println("INLINE case in getText MailClient Please check me Message-ID is " + getID)
      }
      else{
        multiMess.getBodyPart(i).getContent match{
          case s : String=>
            if(match_html.findFirstIn(multiMess.getBodyPart(i).getContentType) !=None) {
              html= s
            }else {
              text = s
            }
          case m : javax.mail.internet.MimeMultipart => getText(m,result)
          case _ => println("Unknow case in getText MailClient Please check me Message-ID is " + getID)
        }
      }

    }

    if(result=="html"){
      if(html==""){
        return text
      }
      return html
    }else {
      return text
    }
  }

  /**
   * Get Files which is attached
   * @return name of attached files
   */
  def getAttachedFilesName = { attachedFilesName }

  /**
   * Get emails which set as From
   * @return List of Address
   */
  def getFrom :List[MailAddress] = {
    var ret_lst:List[MailAddress] = List()

    for( frm <- msg.getFrom){
      ret_lst :::=  List(new MailAddress(frm))

    }
    ret_lst
  }

  /**
   * Get emails which set as ReplyTo
   * @return List of Address
   */
  def getReplyTo = {

    var ret_lst:List[MailAddress] = List()

    for( frm <- msg.getReplyTo){
      ret_lst :::=  List(new MailAddress(frm))

    }
    ret_lst
  }

  /**
   * get Subject of this message
   * @return String name of this message
   */
  def getSubject = { msg.getSubject }

  /**
   * Get number of this message in label
   * @return Int number of this message
   */
  def getMessageNumber = { msg.getMessageNumber }

  /**
   * Get sent date of this message
   * @return Date that this message sent
   */
  def getSentDate = { msg.getSentDate }

  /**
   * Get received date of this message
   * @return Date that this message received
   */
  def getReceivedDate = { msg.getReceivedDate}

  /**
   * Get java mail class
   */
  def getMessageObj : javax.mail.Message =  { msg }



  /**
   * get TEXT/HTML content of this message
   * @return TEXT/HTML content this message
   */
  def getBody():String = {getContent}

  def getTo() = {
    var ret_lst:List[MailAddress] = List()

    for( frm <- msg.getAllRecipients){
      ret_lst :::=  List(new MailAddress(frm))

    }


    ret_lst

  }

  /**
   * Get emails which set as CC
   * @return List of Address
   */
  def getCC()  :List[MailAddress] = {
    //TODO: Parse this header ex Milad Local <milad@local.loc>
    var ret_lst:List[MailAddress] = List()
    val allcc = mime.getHeader("Cc")
    if(allcc!=null){
      for( cc <- allcc){
        ret_lst :::=  List(new MailAddress(new InternetAddress(cc)))
      }
    }

    ret_lst
  }

  /**
   * Get emails which set as BCC
   * @return List of Address
   */
  def getBCC() :List[MailAddress] = {
    var ret_lst:List[MailAddress] = List()
    //TODO: Parse this header ex Milad Local <milad@local.loc>

    val allBcc = mime.getHeader("Bcc")
    if(allBcc!=null){
      for( cc <- allBcc){
        ret_lst :::=  List(new MailAddress(new InternetAddress(cc)))
      }
    }
    ret_lst
  }

  /**
   * Get all the headers for this header name.
   * @return the value fields for all headers with this name
   */
  def getHeader(header:String) = { mime.getHeader(header) }

  /**
   * Get all the headers for this message
   * @return All the headers for this message
   */
  def getHeaders() = {
    //TODO: this should change  now return value is java.util.Enumeration
    mime.getAllHeaderLines
  }
}

/**
 * This class handle mail address and mail's name owner
 */
class MailAddress(in:javax.mail.Address){
  /** java mail class*/
  private val addr = in
  /** java mail class */
  private val int_addr = in.asInstanceOf[InternetAddress]

  /**
   * Get email address
   * @return String of email address
   */
  def getAddress = { int_addr.getAddress }

  /**
   * Get the personal name
   * @return personal name, if personal name is not available return email address
   */
  def getPersonal = { if(int_addr.getPersonal==null) int_addr.getAddress else int_addr.getPersonal }

  /** override toString */
  override def toString = { "MailAddress : " + getAddress }
}

/** General MailClient exception */
trait MailClientException extends Exception {

}

/** Unknown host exception */
class UnknownHostException (host:String) extends MailClientException {
  /** Error code */
  val code = 404
  /** Host which caused error */
  val hostname = host

  /**
   * Get exception reason
   * @return String of error
   */
  def getReason = { "Unknow Host: "+ host}
  override def toString() = { "org.yournex.lib.mail.UnknownHostException "+ getReason}

}

/** Authentication failed exception */
class AuthenticationFailedException(user:String,host:String) extends MailClientException{
  /** Error code */
  val code = 401
  /** Host which caused error */
  val hostname = host
  /** Username which caused error */
  val username = user

  /**
   * Get exception reason
   * @return String of error
   */
  def getReason = { "Authentication Failed for " + username + " on Host "+ host}
  override def toString() = { "org.yournex.lib.mail.AuthenticationFailedException "+ getReason}
}

/** Parse failed exception */
class ParseFailedException(url:String) extends MailClientException {
  /** Error code */
  val code = 300
  /** URL which caused error */
  val connectionUrl = url
  //TODO: Mask password in error
  /**
   * Get exception reason
   * @return String of error
   */
  def getReason = { "Can't Parse conenction " + connectionUrl }
  override def toString() = { "org.yournex.lib.mail.ParseFailedException "+ getReason}

}

/** Connection refused exception */
class ConnectionRefusedException(host:String) extends MailClientException {
  /** Error code */
  val code = 301
  /** Host which caused error */
  val hostname = host

  /**
   * Get exception reason
   * @return String of error
   */
  def getReason = { "Connection refused for HOST :" + hostname }
  override def toString() = { "org.yournex.lib.mail.ConnectionRefusedException "+ getReason}

}
