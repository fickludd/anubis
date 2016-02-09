package se.lth.immun.anubis

import scala.swing._
import actors.Actor
import swing.Publisher
import swing.event.Event
import swing.Swing.onEDT
import swing.Swing.onEDTWait

case class Send(pkt: Any)
case class ActorEvent(pkt: Any) extends Event
case class SwingActorPipe(actor: Actor) extends Actor with Publisher {
  start()

  def act = {
    loop {
      react {
        case Send(pkt) => {
          Console.println("ActorPipe: "+pkt)
          actor ! pkt
        }
        case pkt => 
        	//println("got: "+pkt)
        	onEDT(publish(ActorEvent(pkt)))
        	//publish(ActorEvent(pkt))
      }
    }
  }
  
  def pipe(pkt: Any) = this ! Send(pkt)
}