package taller

import scala.concurrent._
import common._
import scala.util.Random
import java.util.concurrent.{ForkJoinPool, ForkJoinTask, RecursiveTask}
import scala.collection.parallel.immutable.{ParVector, ParSeq}

import scala.concurrent.duration._  

class matriz{ 
  type Matriz = Vector[Vector[Int]]

  def matrizAlAzar(long:Int, vals: Int) : Matriz = {

    val v = Vector.fill(long, long){scala.util.Random.nextInt(vals)}
    v
  }

  def vectorAlAzar(long:Int, vals: Int) : Vector[Int] = {
    val v = Vector.fill(long){scala.util.Random.nextInt(vals)}
    v
  }
  def prodPunto(v1: Vector[Int], v2: Vector[Int]) : Int = {
    (v1 zip v2).map{case (i,j) => i*j}.sum

  }

  def transpuesta(m: Matriz): Matriz = {
    val l = m.length 
    Vector.tabulate(l,l){(i,j) => m(j)(i)}
  }

  def multMatriz(m1: Matriz, m2:Matriz): Matriz = {
    val m2transpuesta = transpuesta(m2)
    val dimension = m1.length

    Vector.tabulate(dimension, dimension){(i,j) => prodPunto(m1(i), m2transpuesta(j))}
  }

  def multMatrizPar(A:Matriz, B:Matriz): Matriz = {
    val Btranspuesta = transpuesta(B)
    val dimension = A.length
    val Task = for {
      i <- 0 until dimension
      j <- 0 until dimension
    } yield task {(i,j,prodPunto(A(i), Btranspuesta(j)))}
    val resultado = Task.map(_.join()).toVector
    Vector.tabulate(dimension, dimension){(i,j) => resultado.find(res => res._1 == i && res._2 == j).map(_._3).getOrElse(0)}

  }

  def subMatriz(m: Matriz, i:Int, j:Int, l:Int): Matriz = {
    Vector.tabulate(l,l){(x,y) => m(i+x)(j+y)}
  }

  def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val dimension = m1.length
    Vector.tabulate(dimension, dimension){(i,j) => m1(i)(j) + m2(i)(j)}
  }

  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    val dimension = m1.length
    if (dimension == 1) {
      Vector(Vector(prodPunto(m1(0), m2(0))))
    } else {
      val mitad = dimension / 2
      val a11 = subMatriz(m1, 0, 0, mitad)
      val a12 = subMatriz(m1, 0, mitad, mitad)
      val a21 = subMatriz(m1, mitad, 0, mitad)
      val a22 = subMatriz(m1, mitad, mitad, mitad)

      val b11 = subMatriz(m2, 0, 0, mitad)
      val b12 = subMatriz(m2, 0, mitad, mitad)
      val b21 = subMatriz(m2, mitad, 0, mitad)
      val b22 = subMatriz(m2, mitad, mitad, mitad)

      val c11 = sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21))
      val c12 = sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22))
      val c21 = sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21))
      val c22 = sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22))

      Vector.tabulate(dimension, dimension){(i,j) =>
        if (i < mitad && j < mitad) c11(i)(j)
        else if (i < mitad && j >= mitad) c12(i)(j - mitad)
        else if (i >= mitad && j < mitad) c21(i - mitad)(j)
        else c22(i - mitad)(j - mitad)
      }
    }
  }

  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    val dimension = m1.length
    if (dimension == 1) {
      Vector(Vector(prodPunto(m1(0), m2(0))))
    } else {
      val mitad = dimension / 2
      val a11 = subMatriz(m1, 0, 0, mitad)
      val a12 = subMatriz(m1, 0, mitad, mitad)
      val a21 = subMatriz(m1, mitad, 0, mitad)
      val a22 = subMatriz(m1, mitad, mitad, mitad)

      val b11 = subMatriz(m2, 0, 0, mitad)
      val b12 = subMatriz(m2, 0, mitad, mitad)
      val b21 = subMatriz(m2, mitad, 0, mitad)
      val b22 = subMatriz(m2, mitad, mitad, mitad)

      val taskc11 = task{sumMatriz(multMatrizRecPar(a11, b11), multMatrizRecPar(a12, b21))}
      val taskc12 = task{sumMatriz(multMatrizRecPar(a11, b12), multMatrizRecPar(a12, b22))}
      val taskc21 = task{sumMatriz(multMatrizRecPar(a21, b11), multMatrizRecPar(a22, b21))}
      val taskc22 = task{sumMatriz(multMatrizRecPar(a21, b12), multMatrizRecPar(a22, b22))}

      val c11 = taskc11.join()
      val c12 = taskc12.join()
      val c21 = taskc21.join()
      val c22 = taskc22.join()


      Vector.tabulate(dimension, dimension){(i,j) =>
          if (i < mitad && j < mitad) c11(i)(j)
          else if (i < mitad && j >= mitad) c12(i)(j - mitad)
          else if (i >= mitad && j < mitad) c21(i - mitad)(j)
          else c22(i - mitad)(j - mitad)
      }
    }
  }

  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val dimension = m1.length
    Vector.tabulate(dimension, dimension){(i,j) => m1(i)(j) - m2(i)(j)}
  }
  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
    val dimension = m1.length
    if (dimension == 1) {
      Vector(Vector(prodPunto(m1(0), m2(0))))
    } else {
      val mitad = dimension / 2
      val a11 = subMatriz(m1, 0, 0, mitad)
      val a12 = subMatriz(m1, 0, mitad, mitad)
      val a21 = subMatriz(m1, mitad, 0, mitad)
      val a22 = subMatriz(m1, mitad, mitad, mitad)

      val b11 = subMatriz(m2, 0, 0, mitad)
      val b12 = subMatriz(m2, 0, mitad, mitad)
      val b21 = subMatriz(m2, mitad, 0, mitad)
      val b22 = subMatriz(m2, mitad, mitad, mitad)

      val m1_ = multStrassen(sumMatriz(a11, a22), sumMatriz(b11, b22))
      val m2_ = multStrassen(sumMatriz(a21, a22), b11)
      val m3_ = multStrassen(a11, restaMatriz(b12, b22))
      val m4_ = multStrassen(a22, restaMatriz(b21, b11))
      val m5_ = multStrassen(sumMatriz(a11, a12), b22)
      val m6_ = multStrassen(restaMatriz(a21, a11), sumMatriz(b11, b12))
      val m7_ = multStrassen(restaMatriz(a12, a22), sumMatriz(b21, b22))


      val c11 = sumMatriz(restaMatriz(sumMatriz(m1_, m4_), m5_), m7_)
      val c12 = sumMatriz(m3_, m5_)
      val c21 = sumMatriz(m2_, m4_)
      val c22 = sumMatriz(sumMatriz(restaMatriz(m1_, m2_), m3_), m6_)
      
      Vector.tabulate(dimension, dimension){(i,j) =>
        if (i < mitad && j < mitad) c11(i)(j)
        else if (i < mitad && j >= mitad) c12(i)(j - mitad)
        else if (i >= mitad && j < mitad) c21(i - mitad)(j)
        else c22(i - mitad)(j - mitad)
      }
    }
  }

  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    val dimension = m1.length
    if (dimension == 1) {
      Vector(Vector(prodPunto(m1(0), m2(0))))
    } else {
      val mitad = dimension / 2
      val a11 = subMatriz(m1, 0, 0, mitad)
      val a12 = subMatriz(m1, 0, mitad, mitad)
      val a21 = subMatriz(m1, mitad, 0, mitad)
      val a22 = subMatriz(m1, mitad, mitad, mitad)

      val b11 = subMatriz(m2, 0, 0, mitad)
      val b12 = subMatriz(m2, 0, mitad, mitad)
      val b21 = subMatriz(m2, mitad, 0, mitad)
      val b22 = subMatriz(m2, mitad, mitad, mitad)

      val m1_ = task{multStrassenPar(sumMatriz(a11, a22), sumMatriz(b11, b22))}
      val m2_ = task{multStrassenPar(sumMatriz(a21, a22), b11)}
      val m3_ = task{multStrassenPar(a11, restaMatriz(b12, b22))}
      val m4_ = task{multStrassenPar(a22, restaMatriz(b21, b11))}
      val m5_ = task{multStrassenPar(sumMatriz(a11, a12), b22)}
      val m6_ = task{multStrassenPar(restaMatriz(a21, a11), sumMatriz(b11, b12))}
      val m7_ = task{multStrassenPar(restaMatriz(a12, a22), sumMatriz(b21, b22))}

      val c11 = sumMatriz(restaMatriz(sumMatriz(m1_.join(), m4_.join()), m5_.join()), m7_.join())
      val c12 = sumMatriz(m3_.join(), m5_.join())
      val c21 = sumMatriz(m2_.join(), m4_.join())
      val c22 = sumMatriz(sumMatriz(restaMatriz(m1_.join(), m2_.join()), m3_.join()), m6_.join())

      Vector.tabulate(dimension, dimension){(i,j) =>
        if (i < mitad && j < mitad) c11(i)(j)
        else if (i < mitad && j >= mitad) c12(i)(j - mitad)
        else if (i >= mitad && j < mitad) c21(i - mitad)(j)
        else c22(i - mitad)(j - mitad)
      }
    }
  }
    
  def prodPuntoParD(v1:ParVector[Int],v2: ParVector[Int]):Int ={
    (v1 zip v2).map{case (i,j) => i*j}.sum
  }

  def compararAlgoritmos(algoritmoSec: (Matriz, Matriz) => Matriz, algoritmoPar: (Matriz, Matriz) => Matriz)(m1: Matriz, m2: Matriz): (Double, Double, Double) = {
    // Medir el tiempo de la versión secuencial
    val tiempoSec = medirTiempo(algoritmoSec(m1, m2))
   
    // Medir el tiempo de la versión paralela
    val tiempoPar = medirTiempo(algoritmoPar(m1, m2))
   
    // Convertir los tiempos a milisegundos
    val tiempoSecMs = tiempoSec / 1e6
    val tiempoParMs = tiempoPar / 1e6
   
    // Calcular la aceleración
    val aceleracion = tiempoSecMs / tiempoParMs
   
    (tiempoSecMs, tiempoParMs, aceleracion)
  }

  // Función para medir el tiempo de ejecución
  def medirTiempo[A](f: => A): Long = {
    val start = System.nanoTime()
    f
    val end = System.nanoTime()
    (end - start)
  }

  // Aquí puedes poner los demás métodos como `multMatrizRec`, `multMatrizRecPar`, etc.
 
  // Método principal para realizar el benchmarking
  def benchmark(): Unit = {
    for (i <- 1 to 10) {
      val size = math.pow(2, i).toInt
      val m1 = matrizAlAzar(size, 2) // Generar matriz aleatoria de tamaño `size x size`
      val m2 = matrizAlAzar(size, 2)

      println(s"\nDimensión de la matriz: $size x $size")

      // Comparar la versión recursiva secuencial y paralela
      val resultadoRec = compararAlgoritmos(multMatriz, multMatriz)(m1, m2)
      println(f"Secuencial: ${resultadoRec._1}%.2f ms, Paralelo: ${resultadoRec._2}%.2f ms, Aceleración: ${resultadoRec._3}%.2f")
   
   // val resultadoStrassen = compararAlgoritmos(multStrassen, multStrassenPar)(m1, m2)
      // println(f"Strassen Secuencial: ${resultadoStrassen._1}%.2f ms, Paralelo: ${resultadoStrassen._2}%.2f ms, Aceleración: ${resultadoStrassen._3}%.2f")
    }
  }
}

object BuscarListaApp {
  def main(args: Array[String]): Unit = {
    val benchmark = new matriz
    benchmark.benchmark()
  }
}
