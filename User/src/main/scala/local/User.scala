package local

import akka.actor._
import scala.io.Source._

object Local extends App {

  implicit val system = ActorSystem("User")
  val localActor = system.actorOf(Props[LocalActor2], name = "LocalActor2")  // the local actor
  var dict = readLine("dicitonary> ")
  var hashes = readLine("hashes> ")
  localActor ! ("start",dict,hashes)                                                     

}

class LocalActor2 extends Actor {

  // create the remote actor
  val remote = context.actorFor("akka://Root@127.0.0.1:5150/user/RemoteActor")
  var timestamp: Long = _
  
  def fileToSet(file:String):Set[String]={
     var s=Set[String]()
     val inFile = scala.io.Source.fromFile(file)
     for(line<-inFile.getLines)
     {
       s+=line
     }
     s
  }
  
  
  def print_time(time:Long)={
     var minutes=time/60
     var seconds=time%60
     var hours=minutes/60
     minutes=minutes%60
     println("total operation took "+hours+":"+minutes+":"+seconds)
  }
  
  def start_crack(dict:String,hashes:String)={
     timestamp=System.currentTimeMillis / 1000
     remote ! "update_bricks"
     println("updating info about workers.......")
     Thread.sleep(2000)
     var s=fileToSet(hashes)
     remote ! ("crack",dict,s)
  }

  def receive = {
    case ("start",dict:String,hashes:String) =>
        timestamp=System.currentTimeMillis / 1000
        remote ! "update_bricks"
        println("updating info about workers.......")
        Thread.sleep(2000)
        var s=fileToSet(hashes)
        remote ! ("crack",dict,s)
    case ("cracked",hash:String,password:String)=>
       println("cracked "+hash+"  =  "+password)
    case ("msg",message:String)=>
       println(message)
    case "end" =>
       println("We have ended job")
       print_time(System.currentTimeMillis / 1000-timestamp)
       var dict = readLine("dicitonary> ")
       var hashes = readLine("hashes> ")
       start_crack(dict,hashes)
       
    case _ =>
        println("odebrano")
        
  }
} 
