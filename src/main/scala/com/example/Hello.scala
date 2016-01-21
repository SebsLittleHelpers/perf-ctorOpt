package com.example

import scala.scalajs.js.JSApp
import org.scalameter.api._
import org.scalameter.picklers.noPickler._
import org.scalameter.utils.Tree
import org.scalameter.CurveData

object Hello extends JSApp {


  object CtorOptBench extends LocalTime {
    class A(message: String, id: Int) {
      override def toString: String = s"$id -> $message"
    }

    class B {
      override def toString: String = "just a B"
    }

    class C(a: A, b: B) {
      override def toString: String = s"($a, $b)"
    }

    def dummyList(): List[(String, Int)] = {
      List.fill(40000)(1).map(x => (s"$x", x))
    }

    val aLotOfElements = Gen.unit("sample").map(_ => dummyList()).cached

    performance of "CtorOpt" in {
      measure method "new A" in {
        using(aLotOfElements) in { list =>
          list.foreach {
            case (str, num) =>
              new A(str, num).toString
          }
        }
      }

      measure method "new B" in {
        using(aLotOfElements) in { list =>
          list.foreach {
            case (str, num) =>
              new B().toString
          }
        }
      }

      measure method "new C" in {
        using(aLotOfElements) in { list =>
          list.foreach {
            case (str, num) =>
              new C(new A(str, num), new B()).toString
          }
        }
      }
    }
    
    override def reporter = new Reporter[Double] {
      def report(result: CurveData[Double], persistor: Persistor): Unit = {
        println(s"\nOne Test finished : success = ${result.success}")

        println("\nContext:")
        val ctx = result.context
        ctx.properties.foreach { x => println(s"${x._1} -> ${x._2}") }

        println("\nInfo:")
        val info = result.info
        info.foreach { x => println(s"${x._1} -> ${x._2}") }

        println("\nMeasurements:")
        val measurements = result.measurements
        measurements.foreach { x =>
          val axisData = x.params.toString
          val result = x.value + x.units
          println(s"$axisData : $result")
        }

        val measure = math.round(measurements.head.value * 100) / 100
        val units = measurements.head.units

        val id = ctx.scopeList.mkString("-")
        println(s"id = $id")
        //dom.document.getElementById(id).innerHTML = s"$measure $units"
      }

      def report(results: Tree[CurveData[Double]], persistor: Persistor): Boolean = {
        println("\n\nAll Test finished : ")
        var total = 0.0;
        for(res <- results; measure <- res.measurements){
          val axisData = measure.params.toString
          val result = measure.value + measure.units
          println(s"$axisData : $result")
          total += measure.value
        }

        println(s"Total : $total")

        true
      }

    }

  }

  def main(): Unit = {
    CtorOptBench.main(Array())
  }
}

