package local

import akka.actor._
import java.io.File
import java.io.FileInputStream
import java.nio.channels.FileChannel.MapMode._
import java.security.MessageDigest
import java.nio._

object Local extends App {

  implicit val system = ActorSystem("Brick")
  val localActor = system.actorOf(Props[LocalActor], name = "LocalActor")  // the local actor
  localActor ! "START"                                                     // start the action

}

class LocalActor extends Actor {

  // create the remote actor
  val remote = context.actorFor("akka://Root@127.0.0.1:5150/user/RemoteActor")

  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes)
  }

  
  def Eq(a1: Array[Byte],a2: Array[Byte])=
  {
    var Y:Boolean = true
    var i:Int =0
    while(i<16)
    {
      if(a1(i)!=a2(i))
      {
        Y=false
      }
      i+=1
    }
    Y
  }
  
  def charToInt(c: Char)={
    if (c>='a' && c<='f')
      c-87
    else
      c-48
  }
  
  def hashToArray(s:String )={
    val a = new Array[Byte](16)
    for(i<- 0 to 15)
    {
      a(i)=(charToInt(s(i*2))*16+charToInt(s(i*2+1))).toByte
    }
    a
  }
  
  def hashToString(a: Array[Byte])={
    a.map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}
  }
  
  def printHash(a: Array[Byte])={ //debug
    for(i<- 0 to 15)
    {
      print(a(i))
      print(" ")
    }
    println("")
  }
  
  def hashHash(a:Array[Byte]):Int={
    val hashChars=6 //option 16*16*16*16*16*16=16777216   very much for database 
    var r:Int=0
    for(i<-0 to hashChars)
    {
      r=r*16+a(i)
    }
    r
  }
  
  def addHash(m:Map[Int,List[Array[Byte]]],s:String):Map[Int,List[Array[Byte]]]={
   if(s.size==32)
   {
    val a:Array[Byte]=hashToArray(s)
    val hash:Int=hashHash(a)
    if(m.contains(hash))
    {
      m+(hash->(  m(hash):+a  ))
    }
    else
    {
      m+(hash->List(a))
    }
   }
   else
    m
  }
  
  def initMap(hashes:Set[String])={
    var m=Map[Int,List[Array[Byte]]]()
    for(hash<-hashes)
    {
      m=addHash(m,hash)
    }
    m
  }
  
  
  def iTc(i:Int)={
    i.asInstanceOf[Char]
  }
  
  //java.nio.DirectByteBufferR
  def checkChunk(buffer:ByteBuffer,rest:String,size:Long,forCrack:Map[Int,List[Array[Byte]]]):String={
    var line=rest
    for(i <- 1 until size.toInt)
        {
          if(buffer.get(i)<32)
          {
            //try line
            
            val H=md5(line)
            val hashH=hashHash(H)
            if(forCrack.contains(hashH))
            {
              for(a<-forCrack(hashH))
              {
                if(Eq(a,H))
                {
                   //println("cracked "+hashToString(a)+"  =  "+line)
                   sender ! ("cracked",hashToString(a),line)
                
                }
              }
            }
            
            line=""
          }
          else
          {
            line+=iTc(buffer.get(i))
          }
        }
        line
  }
  
  def receive = {
    case "START" =>
        remote ! "register"
    case "ping" =>
        remote ! "register"
    case ("crack",dict:String,hashes:Set[String],fromm:Long,sizee:Long)=>
        var size:Long=sizee
        var from:Long=fromm//interesting
        println(from)
        println(size)
       
       
        var forCrack=initMap(hashes)
        val file = new File(dict)
        val stream =new FileInputStream(file)
        var rest:String=""
        val chunk_size=1*1024*1024*50//50MB
        while(size>0)
        {
          var nsize=size
          if(nsize>chunk_size)
            nsize=chunk_size
          size-=nsize
          var buffer=stream.getChannel.map(READ_ONLY,from,nsize)
          rest=checkChunk(buffer,rest,nsize,forCrack)
          from+=nsize
        }
        
        remote ! "end"
        println("")
    case _ =>
        println("odebrano nie wiadomo co")
  }
} 
