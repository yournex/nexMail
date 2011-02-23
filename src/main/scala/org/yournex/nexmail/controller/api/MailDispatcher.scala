package org.yournex.nexmail.controller.api

/**
 * Created by IntelliJ IDEA.
 * User: apple
 * Date: 2/13/11
 * Time: 7:41 PM
 * To change this template use File | Settings | File Templates.
 */

import javax.servlet.http.{HttpServlet,
  HttpServletRequest => HSReq, HttpServletResponse => HSResp}

import org.yournex.nexmail.view._
import org.yournex.nexmail.model._
import org.yournex.lib.mail._
import org.yournex.nexmail.actor._

import scala.util.control.Exception

class MailDispatcher extends HttpServlet {
  override def doGet(req : HSReq, resp : HSResp) = {

    //TODO : check about exisiting object
    var sessionId= req.getRequestedSessionId

    var json_res = ""
    if(req.getParameter("action") == "getLabels")
      json_res = getLabels(sessionId)
    else if (req.getParameter("action")== "getMessages"){
      val label = if(req.getParameter("label") != null)
                    req.getParameter("label")
                  else
                    "INBOX"
      val from =  if(req.getParameter("from") != null)
                    req.getParameter("from")
                  else
                    "1"
      val to =  if(req.getParameter("to") != null)
                    req.getParameter("to")
                  else
                    "50"
      json_res = getMessages(sessionId,label ,from,to)


    }


    resp.getWriter().print(json_res)
  }

  def getMessages ( sessionId:String ,label:String, from:String,to:String): String = {
    //catch exception if it could not convert to int
    val m_from = from.toInt
    val m_to   = to.toInt

    val msgs = NexMailActor.getInstance(sessionId).get.getMessages(label,m_from,m_to)


    APITemplate.labelMessages(label,msgs)

  }

  def getLabels(sessionId:String) : String = {
    val labelmap =  (NexMailActor.getInstance(sessionId).get.getLabels)

    APITemplate.labels(labelmap)
  }
}

