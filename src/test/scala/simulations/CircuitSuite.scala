package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }
  
  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
    
    in2.setSignal(false)
    run
    
    assert(out.getSignal === true, "or 4")
    
  }
  
   test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
    
    in2.setSignal(false)
    run
    
    assert(out.getSignal === true, "or 4")
    
  }
   
   test("demultiplexer test simple case"){
     val in = new Wire
     val c:List[Wire] = List()
     val out = List(new Wire)
     demux(in, c, out)
     in.setSignal(true)
     run
     assert(out(0).getSignal === true, "demux simple 1")
     
     in.setSignal(false)
     run
     assert(out(0).getSignal === false, "demux simple 1")
   }
   test("case input is false"){
     val in = new Wire
     val c:List[Wire]= List(new Wire, new Wire)
     val out:List[Wire] = List(new Wire, new Wire, new Wire, new Wire)
     demux(in, c, out)
     in.setSignal(false)
     run
     assert(didListHasAllFalseWires(out) === true, "demux 1")
    
    
   }
   
   test("complex test"){
      val in = new Wire
     val c:List[Wire]= List(new Wire(), new Wire)
     val out:List[Wire] = List(new Wire, new Wire, new Wire, new Wire)
     demux(in, c, out)
     in.setSignal(true)
     c(0).setSignal(true)
     run
     
     assert(out(2).getSignal === true, "complex demux after setSignal to 1, and control = 10")
     assert(didListHasAllFalseWires(out.slice(0, 2)) === true )
     assert(out(3).getSignal === false)
      
     in.setSignal(false) 
     run
     
     assert(didListHasAllFalseWires(out) === true, "complex demux after setSingal to 0")
     
  }
   
   test("test getIndexOutput"){
     val c:List[Wire] = List()
     val w = new Wire
     w.setSignal(true)
     val newList = c :+ w
     val newList2 = newList :+ new Wire
     assert(getIndexOutput(newList2) == 2)
   }

    def didListHasAllFalseWires(list:List[Wire]): Boolean = {
       list.filter({c:Wire => c.getSignal == true}).isEmpty 
     }
}
