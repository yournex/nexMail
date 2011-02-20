package org.yournex.nexmail.actor

/**
 * Created by IntelliJ IDEA.
 * User: apple
 * Date: 2/10/11
 * Time: 8:10 PM
 * To change this template use File | Settings | File Templates.
 */

import scala.actors.Actor
import org.yournex.nexmail.model.NexMail

class MailContainer extends NexMail with Actor {
     var isLogined = false
     override def login(username:String ,password:String, server:String, protocol:String): Boolean = {
      if(isLogined==false){
        super.login(username, password, server, protocol)
      }
      isLogined = true
      true
     }

     def act() = {

     }
}

object NexMailActor extends Actor      {
  var mailContainer = Map.empty[String,MailContainer]
  start

  var value = 1
  def act() = {
    loop{
      value += 1
      Thread.sleep(2000)
    }
  }

  def addInstance(mc:MailContainer, sessionId:String): Boolean = {
    //TODO: this if is not working !
    if( (mailContainer filter { case (key,value) =>  mc.username == value.username } size) >1 ){
      return false
    }
    mailContainer  += (sessionId -> mc)
    true
  }

  def  getInstance(sessionId:String) :Option[MailContainer] = {
    if ( mailContainer.get(sessionId) == None )
      return None
    mailContainer.get(sessionId)
  }

}