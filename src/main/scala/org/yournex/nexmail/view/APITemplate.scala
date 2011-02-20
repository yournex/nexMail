package org.yournex.nexmail.view

/**
 * Created by IntelliJ IDEA.
 * User: Milad
 * Date: 2/12/11
 * Time: 2:31 PM
 * To change this template use File | Settings | File Templates.
 */

import org.yournex.lib.mail._

object APITemplate {
  def labelMessages(label:String,msgs:List[MailMessage]):String = {
    var ret_str = """{
          "stat" : "ok"
          "data" : {
                    "label"    :"""+"\"" + label + "\"" +"""
                    "messages" :{"""

    for ( msg <- msgs ) {

      ret_str += format (""" {"subject":"%s", , "body":"%s" ,"from": {""",msg.getSubject,msg.getBody)
      for(frm <- msg.getFrom){
        ret_str += format(""" {"%s":"%s"}, """,frm.getPersonal, frm.getAddress)
      }
      ret_str += "} },"
    }

    ret_str += """}
              }
          }
        """

    ret_str
  }
  def labels(labelName: Map[String,List[Int]]):String = {
    var ret_str ="""{
      "stat" : "ok",
      "data" : ["""

    for ( (name,num) <- labelName) {
      ret_str += format(""" {"%s":{"unread":"%s", "all":"%s"}} ,""",name,num(0),num(1))
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
