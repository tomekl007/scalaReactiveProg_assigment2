package simulations

import math.random
import scala.collection.mutable.MutableList

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    //val speedOfSimulation = 1 //one action, one day pass
    val prevalenceRate = 0.01
    val maxDaysAfterPersonShouldMove = 5
    //copied from EpidemySuite
    val incubationTime = 6
    val dieTime = 14
    val immuneTime = 16
    val healTime = 18

      
    val transRate = 0.4
    val dieRate = 0.25
    
    val stepOfPeopleToInfect  = population / (population * prevalenceRate)

  }

  import SimConfig._

  val persons: List[Person] = createPopulation()
  def createPopulation():List[Person] =  {
    def loop(list:List[Person], id:Int):List[Person] ={
    	if(id > SimConfig.population) list
    	else if(id % SimConfig.stepOfPeopleToInfect == 0) {
    	  val infecedPerson = new Person(id)
    	  infecedPerson.infectPerson(infecedPerson)//change it, extract to PersonActions maybe
    	  loop(list :+ infecedPerson, id + 1)
    	}
      	else loop(list :+ new Person(id), id + 1)
      }
	  loop(List(), 1)
	}
  println(persons)
  //temp start moving persons
  startMovingPersons()
  def startMovingPersons(){
    persons.foreach(p => p.moveToNeightbourRoom(p))
  }
  
 
  
  
  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    private var actions: List[Simulator#Action] = List()
    
    override def toString:String = {
       s"person id : $id, infected : $infected , x : $row, y: $col ";
    }
    
     def addAction(a: Simulator#Action) {
    	actions = a :: actions
    	a()
    }
    
    
    
   def moveToNeightbourRoom(person:Person) = {
     def moveAction():Unit = {
       val randomDelay = randomBelow(SimConfig.maxDaysAfterPersonShouldMove)
       afterDelay(randomDelay){
         val neightbours:List[(Int, Int)] = getNeightboursForRoom(person.row, person.col)
         val randomRoom = randomBelow(4)
         println("choosing neight from "  + neightbours + " index : " + randomRoom)
         val coordinateRoomToMove = neightbours(randomRoom)
         if(!roomHasDeadOrSickPersons(coordinateRoomToMove) && person.dead != true){
            println(s"person $person move to room : $coordinateRoomToMove")
           person.row = coordinateRoomToMove._1
           person.col = coordinateRoomToMove._2
         
	         if(roomHasInfectedPerson(person.row-> person.col) && shouldBeInfeced()
	             && person.immune == false && person.infected == false){
	          println(s"$person is getting infeced")
	          person.infectPerson(person)
	         }
         }
          person addAction(moveAction)
       }
      
     }
     person addAction(moveAction)
   }
   
   def shouldBeInfeced():Boolean = {
     val randomNumber = randomBelow(100)
     if(randomNumber < 40){
       println("--------------> " )
       return true
     }else{
       return false
     }
   }
   
    def infectPerson(person:Person){
    def infectAction():Unit = {
      person.infected = true
      afterDelay(SimConfig.incubationTime){
        println("after " + SimConfig.incubationTime + s"seconds, person $person change state to sick")
        person.sick = true
      }
    }
    def deadAction():Unit = {
      afterDelay(SimConfig.dieTime){
    	  println("infected person could die with propability 25%");
    	  val didDie = randomBelow(4) == 1
    	  if(didDie){
    	    println(s"$person will die")
    	    person.dead = true
    	  }
      }
    }
    def immuneAction():Unit = {
      afterDelay(SimConfig.immuneTime){
        if(person.dead != true){
        	println("person became immune after " + SimConfig.immuneTime + "days" )
        	person.immune = true
        	person.sick = false
        	//person.infected = false
        }
      }
    }
    def healAction():Unit = {
      afterDelay(SimConfig.healTime){
        println(s"$person after " + SimConfig.healTime + " became healty again")
        if(person.dead != true){
         person.infected = false
         person.sick = false
         person.immune = false
        }
      }
    }
   
    person addAction(infectAction) 
    person addAction(deadAction)
    person addAction(immuneAction)
    person addAction(healAction)
  }
   
  }
  def roomHasInfectedPerson(coords:(Int, Int)):Boolean ={
    persons.foreach({
      p:Person => {
        if(compareCoordinates(coords, p) && p.infected == true){
          println(s"room : $coords has infected person inside")
          return true
        }
        
      }
    })
    false
  }
  
  def roomHasDeadOrSickPersons(coords:(Int, Int)):Boolean ={
    persons.foreach({
      p:Person => {
        if(compareCoordinates(coords, p) && p.sick == true || p.dead ==  true){
          println(s"room $coords, contains person which is sick or dead")
          return true;
        }
      } 
    })
    return false;
  }
  
  //._1 - row ._2 - column
   def getNeightboursForRoom(row:Int, col:Int) : List[(Int, Int)] = {
    var list:List[(Int, Int)] = List()
     
    val firstPair = (row % 8) -> (col-1) % 8
     if(firstPair._2 == -1){
       list = list :+ (row % 8, 7)
     }else{
       list = list :+ firstPair
     }
    list = list :+ ((row % 8) -> (col +1) % 8)
    
     val secondPair = ((row - 1) % 8 -> col % 8)
     if(secondPair._1 == -1){
       list = list :+ (7, col % 8)
     }else{
       list = list :+ secondPair
     } 
    list = list :+ ((row + 1) % 8 -> col % 8)
    
     //it should be like this: 
     //List(((row % 8) -> (col-1) % 8), ((row % 8) -> (col +1) % 8)
     //    , ((row - 1) % 8 -> col % 8), ((row + 1) % 8 -> col % 8))
     list
    //List(((row % 8) -> (col) % 8), ((row % 8) -> (col +1) % 8)
    //     , ((row ) % 8 -> col % 8), ((row + 1) % 8 -> col % 8))
   }
  
  private def compareCoordinates(coords: (Int, Int), p: EpidemySimulator.this.Person): Boolean = {
    p.row == coords._1 && p.col == coords._2
  }
   
}
