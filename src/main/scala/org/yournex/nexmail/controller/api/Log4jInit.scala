package org.yournex.nexmail.controller.api

import org.apache.log4j.PropertyConfigurator;
import org.apache.log4j.Logger;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.PrintWriter;
import java.io.IOException;

class Log4jInit extends HttpServlet {
    override def init() = {
        val prefix =  getServletContext().getRealPath("/");
        val file = getInitParameter("log4j-init-file");
        if( file != null){
            PropertyConfigurator.configure(prefix+file);
        }
    }
    override def doGet(req: HttpServletRequest, res:HttpServletResponse) = {
    }
}
