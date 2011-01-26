//package org.yournex.nexmail.mailclient

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



class MailClient {

  val supportedProtocol = List("IMAPS")
  val defaultLabel      = "Inbox"

  var selectedProtocol: String = ""
  var selectedUsername: String = ""
  var selectedPassword: String = ""
  var selectedServer  : String = ""
  var selectedPort    : String = ""
  var selectedFolder: String   = ""

  var store: javax.mail.Store = null

  //url : IMAPS://username:password@imap.domain.com:993
  def getInstance(url: String) = {
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
  }

  def getLabels() ={
  }

  def getMSG(label: String) ={

  }

  def createLabel(label: String) :Boolean = {
   val inbox =  store.getFolder("Scala");
   inbox.create(1)

  }

}

//not now
class MailMSG {

}

