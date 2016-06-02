import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.actor.ActorDSL.inbox
import akka.pattern.ask
import scala.concurrent.duration.DurationInt

/**
  * %%%-------------------------------------------------------------------
  * %% A ring benchmark.
  * %% Create N processes in a ring.
  * %% Send a message round the ring M times so that a total of N * M messages get sent.
  * %% Measure Time how long this takes for different values of N and M.
  * %% %%%------------------------------------------------------------------- */

object Ring {
  //
  //  case class CreateRing(from: ActorRef, n: Int)
  //
  //  case object Done
  //
  //  case class Message(ring: ActorRef, from: ActorRef,)
  //

  def main(args: Array[String]) {
    val n = args(0).toInt
    val m = args(1).toInt

    println(s"Ring Benchmark. $n processes, sent $m times ")
    implicit val system = ActorSystem("Ring")
    val ring = system.actorOf(Props[Proc])
    implicit val i = inbox()
    ring ! ("create_ring", i.getRef(), n)
    val r = i.receive(timeout = 1.minute)

    val lastNode = r match {
      case (last: ActorRef, "ring_created") =>
        println("Sending message")
        last
      case m : Any =>
        println(s"Wrong message received: $m")
        throw new Exception(s"Wrong message received: $m")
    }

    val t0 = System.currentTimeMillis()
    ring ! ("message", ring, lastNode, m)
    val resp = i.receive(60.seconds)
    val t1 = System.currentTimeMillis()

    if (resp == "done")
      println("OK received " + resp)
    else
      println("ERROR")

    val time = t1 - t0

    println(s"Done in $time milliseconds. Ring with $n actors, message sent $m times")

//    Thread.sleep(3000)
    ring ! PoisonPill

    System.exit(0)
  }

}

class Ring {

}


class Proc() extends Actor {

  def receive = init

  def init: Receive = {
    case ("create_ring", root: ActorRef, 0) =>
      println("Ring Created!")
      root !(self, "ring_created")
      context.become(ready(root))
    case ("create_ring", root: ActorRef, n: Int) =>
      val pid = context.actorOf(Props[Proc])
      pid !("create_ring", root, n - 1)
      context.become(ready(pid))
  }

  def ready(next: ActorRef): Receive = {
    case ("message", _begin: ActorRef, end: ActorRef, 0) if end.equals(self) =>
      next ! "done"
    case ("message", begin: ActorRef, end: ActorRef, 0) =>
      next !("message", begin, end, 0)
    case ("message", begin: ActorRef, end: ActorRef, m: Int) if end.equals(self) =>
//      println(s"Message passed all ring")
      begin ! ("message", begin, end, m - 1)
    case ("message", begin: ActorRef, end: ActorRef, m: Int) =>
      next ! ("message", begin, end, m)
    case m: Any =>
      println("ERROR: Received message " + m)
  }

}