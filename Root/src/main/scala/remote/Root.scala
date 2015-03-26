package remote

import akka.actor._
import akka.remote._
import java.io.File
import java.io.FileInputStream
import java.nio.channels.FileChannel.MapMode._

object HelloRemote extends App  {
  //val system = ActorSystem("HelloRemoteSystem")
  val system = ActorSystem("Root")
  val remoteActor = system.actorOf(Props[RemoteActor], name = "RemoteActor")
  remoteActor ! "run"
}

class RemoteActor extends Actor {
  var bricks=Set[ActorRef]()
  var running:Boolean=false
  var working_bricks:Int=0
  var hashes_len:Int=0
  var user:ActorRef=_
  def receive = {
    case "run"=>
       println("the Root Server is running")
    case "register"=>
       println("new Brick ")
       bricks+=sender
    case "unregister"=>
       println("one Brick go away")
       bricks-=sender
    case "update_bricks"=>
       if(running)
          sender ! ("msg","we are still working")
       else
       {
          for (brick<-bricks)
          {
            brick ! "ping"
          }
          bricks=Set[ActorRef]()//all nodes must to register again - this is another form of ping 
       }
    case ("crack",dict:String,hashes:Set[String])=>
       if(running)
          sender ! ("msg","we are still working")
       else
       {
          running=true
          user=sender
          working_bricks=bricks.size
          hashes_len=hashes.size
          println("started cracking "+hashes_len+" passwords with "+bricks.size+" machines")
          sender ! ("msg","started cracking "+hashes_len+" passwords with "+bricks.size+" machines")
          val file = new File(dict)  
          val fileSize=file.length
          println("dictionary has "+fileSize+" bytes")
          sender ! ("msg","dictionary has "+fileSize+" bytes")
          val poIle:Long=fileSize/bricks.size
          var ile1:Long=fileSize%bricks.size
          var ost:Long=0
          for(brick <-bricks)
          {
            var d:Long=poIle
            if(ile1>0)
            {
              d+=1
              ile1-=1
            }
            brick ! ("crack",dict,hashes,ost,d)
            ost+=d
          }
       }//if running==false
    case ("cracked",hash:String,password:String)=>
       user ! ("cracked",hash,password)
    case "end"=>
       working_bricks-=1
       if(working_bricks>0)
         user ! ("msg","One worker has everything done")
       else
       {
         user ! ("end")
         running=false
       }
    case _ =>
       println("Root received unknown message ")
  }
} 

