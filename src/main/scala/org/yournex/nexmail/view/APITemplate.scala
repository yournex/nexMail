package org.yournex.nexmail.view

/**
 * Created by IntelliJ IDEA.
 * User: Milad
 * Date: 2/12/11
 * Time: 2:31 PM
 * To change this template use File | Settings | File Templates.
 */

import com.twitter.json._

import org.yournex.lib.mail._

object APITemplate {
  def labelMessages(label:String,msgs:List[MailMessage]):String = {
    var ret_str = """{
          "stat" : "ok",
          "data" : {
                    "label"    :"""+"\"" + label + "\"," +"""
                    "messages" :["""

    for ( msg <- msgs ) {

      msg.getTo

      ret_str += format ("\n"+"""[{"id":"%s","subject":"%s",  "body":%s ,"sentDate":"%s", "receivedDate":"%s" ,"from": [""",msg.getID,msg.getSubject,Json.quote(msg.getBody),msg.getSentDate,msg.getReceivedDate)
      //FROM
      for(frm <- msg.getFrom){
        ret_str += format(""" {"fullname":"%s","email":"%s"}, """,frm.getPersonal, frm.getAddress)
      }
      ret_str += "] "
      //ReplyTO
      ret_str += ", \"replyTo\":["
      for(rpl <- msg.getReplyTo){
        ret_str += format(""" {"fullname":"%s","email":"%s"}, """,rpl.getPersonal, rpl.getAddress)
      }
      ret_str += "]"

      //TO
      ret_str += ", \"to\":["
      for(to <- msg.getTo){
        ret_str += format(""" {"fullname":"%s","email":"%s"}, """,to.getPersonal, to.getAddress)
      }
      ret_str += "]"

      //CC
      ret_str += ", \"cc\":["
      for(cc <- msg.getCC){
        ret_str += format(""" {"fullname":"%s","email":"%s"}, """,cc.getPersonal, cc.getAddress)
      }
      ret_str += "]"

      //BCC
      ret_str += ", \"bcc\":["
      for(bcc <- msg.getBCC){
        ret_str += format(""" {"fullname":"%s","email":"%s"}, """,bcc.getPersonal, bcc.getAddress)
      }
      ret_str += "]"

      //attached file
      ret_str += ", \"attachedFiles\":["
      for(afile <- msg.attachedFilesName){
        ret_str += format(""" "%s", """,afile)
      }
      ret_str += "]"


      ret_str +="} ],\n"

    }

    ret_str += """]
              }
          }
        """
    ret_str
  }

  def labels(labels: Map[String, MailLabel]):String = {
    var ret_str ="""{
      "stat" : "ok",
      "data":["""

    labels foreach {
      case(name,lbl) => if(lbl.getUnreadMessageCount() == -1 && lbl.getMessageCount() == -1){
        ret_str += format(""" {"name": "%s", "unread":"-1", "all":"-1","sub_label":[%s]}, """,name,gaugingLabel(lbl.getSubLabels))
      }else{
        ret_str += format(""" {"name": "%s", "unread":"%s", "all":"%s","sub_label":[]}, """,name,lbl.getUnreadMessageCount(),lbl.getMessageCount())
      }

    }

    def gaugingLabel(subs: Map[String, MailLabel]) : String={
      var ret_str = ""
      subs foreach{
        case(name,lbl) =>if(lbl.getUnreadMessageCount() == -1 && lbl.getMessageCount() == -1){
          ret_str +=format(""" {"name": "%s", "unread":"-1", "all":"-1","sub_label":[%s]}, """,name,gaugingLabel(lbl.getSubLabels))
        }else{
          ret_str += format(""" {"name": "%s", "unread":"%s", "all":"%s","sub_label":[]}, """,name,lbl.getUnreadMessageCount(),lbl.getMessageCount())
        }
      }
      return ret_str
    }

    ret_str += """ ] }"""

    ret_str
  }

  def failed(code:Int,reason:String): String ={
    format("""{
      "stat":"fail",
      "err":{
            "code":"%s",
            "msg" : "%s"
            }
    }""", code ,reason)
  }

  def loginFailed(reason:AuthenticationFailedException) :String = {

    format("""{
      "stat":"fail",
      "err":{
            "code":"%s",
            "msg" : "%s"
            }
    }""",  reason.code ,reason.getReason)

  }

  def unknownHost(reason:UnknownHostException) :String= {
    format("""{
      "stat":"fail",
      "err":{
            "code":"%s",
            "msg" : "%s"
            }
    }""", reason.code , reason.getReason)
  }

  def loginSuccessful :String = {
    format("""{"stat" : "ok"}""")
  }

}


