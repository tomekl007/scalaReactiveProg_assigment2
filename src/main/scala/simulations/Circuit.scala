package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
  
   override def toString:String = {
    sigVal.toString
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction(): Unit = {
      val in1Sig = a1.getSignal
      val in2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output setSignal (in1Sig | in2Sig )}
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
      val notIn1, notIn2, notOut = new Wire
      inverter(a1, notIn1); inverter(a2, notIn2)
      andGate(notIn1, notIn2, notOut)
      inverter(notOut, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    def demuxAction(): Unit = {
    	val inSig = in.getSignal
    	if(inSig == false){//set all output to false
    	   afterDelay(0){ for(o <- out) o.setSignal(false)} 
    	}else if(c.isEmpty){//set output to input
    		afterDelay(0){ out(0).setSignal(inSig) }
    	}else{
    	  afterDelay(0){
    		val indexOfOutputToSet = getIndexOutput(c)
            println("index -> " + indexOfOutputToSet)
    	  	out(indexOfOutputToSet).setSignal(inSig)
    	  }
    	  
    	}
    }
    in addAction demuxAction
  }
  def getIndexOutput(list:List[Wire]): Int = {
    println("get index for : " + list)
		  def loop(ls:List[Wire], acc:String):String = {
		    if(ls.isEmpty) acc
		    else loop(ls.tail, acc + (if (ls.head.getSignal == true) 1 else 0))
		  }
    Integer.parseInt(loop(list,""), 2)
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }
  
  def orGateExample {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
  Circuit.orGateExample
}
