package org.yournex.nexmail.controller.api

/**
 * Created by IntelliJ IDEA.
 * User: apple
 * Date: 2/10/11
 * Time: 12:22 AM
 * To change this template use File | Settings | File Templates.
 */

import javax.servlet.http.{HttpServlet,
  HttpServletRequest => HSReq, HttpServletResponse => HSResp}

import org.yournex.nexmail.view._
import org.yournex.nexmail.model._
import org.yournex.lib.mail._
import org.yournex.nexmail.actor._

import scala.util.control.Exception

class TooMuchThreadPerUser(user:String) extends  Exception {
  val code = 500
  val username = user
  def getReason = { "user " + username + " has more thread than suppose has"}
  override def toString() = { "org.yournex.nexmail.controller.api.TooMuchThreadPerUser "+ getReason}
}

class LoginAuth extends HttpServlet {
  override def doGet(req : HSReq, resp : HSResp) = {
    if(req.getRequestedSessionId == null)
      req.getSession
    //TODO: if session is not exisit it will return empty we should find the way that to change it or update it fast

    var mailbox =if(NexMailActor.getInstance(req.getRequestedSessionId) == None){
      new MailContainer
    }else {
      NexMailActor.getInstance(req.getRequestedSessionId).get
    }

    var json_res = " "
    try {
      var x = new NexMail()
      if(mailbox.login(req.getParameter("username"), req.getParameter("password"), req.getParameter("server"), req.getParameter("protocol"))){
        if(NexMailActor.addInstance(mailbox,req.getRequestedSessionId)){
          json_res = APITemplate.loginSuccessful
        }else {
          throw new TooMuchThreadPerUser(req.getParameter("username"))
        }
      }

    }catch {
      case e:UnknownHostException => json_res = APITemplate.unknownHost(e)
      case e:AuthenticationFailedException => json_res = APITemplate.loginFailed(e)
      case e:ParseFailedException => json_res = APITemplate.failed(e.code,e.getReason)
      case e:ConnectionRefusedException => json_res = APITemplate.failed(e.code,e.getReason)
      case otherException => throw otherException
   }


    println(NexMailActor.mailContainer)
    resp.getWriter().print(json_res)
    //"index is 2 = " + req.getParameter("server") +

  }

  override def doPost(req : HSReq, resp : HSResp) = {
    resp.getWriter().print("<HTML>" +
    "<HEAD><TITLE>Hello, Scala!</TITLE></HEAD>" +
    "<BODY>Hello, Scala! This is a servlet.</BODY>" +
    "index is = " +
    "</HTML>")
  }

}
