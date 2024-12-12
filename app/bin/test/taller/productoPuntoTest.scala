package taller

import org.scalatest.funsuite.AnyFunSuite
import scala.util.Random

class MatrizTest extends AnyFunSuite {
  val objMatriz = new matriz()

  // Generar una matriz aleatoria para pruebas
  def matrizAleatoria(size: Int, maxValor: Int): Vector[Vector[Int]] =
    Vector.fill(size, size)(Random.nextInt(maxValor))

  // Test para la función prodPunto
  test("Producto punto - resultado correcto") {
    val v1 = Vector(1, 2, 3)
    val v2 = Vector(4, 5, 6)

    val resultadoEsperado = 1 * 4 + 2 * 5 + 3 * 6
    val resultado = objMatriz.prodPunto(v1, v2)

    assert(resultado == resultadoEsperado, s"El producto punto no es correcto: $resultado vs $resultadoEsperado")
  }

  // Test para la función transpuesta
  test("Transpuesta - resultado correcto") {
    val m = Vector(
      Vector(1, 2, 3),
      Vector(4, 5, 6),
      Vector(7, 8, 9)
    )
    val resultadoEsperado = Vector(
      Vector(1, 4, 7),
      Vector(2, 5, 8),
      Vector(3, 6, 9)
    )
    val resultado = objMatriz.transpuesta(m)

    assert(resultado == resultadoEsperado, "La transpuesta no es correcta")
  }

  // Test para la función suma de matrices
  test("Suma de matrices - resultado correcto") {
    val m1 = Vector(
      Vector(1, 2, 3),
      Vector(4, 5, 6),
      Vector(7, 8, 9)
    )
    val m2 = Vector(
      Vector(9, 8, 7),
      Vector(6, 5, 4),
      Vector(3, 2, 1)
    )
    val resultadoEsperado = Vector(
      Vector(10, 10, 10),
      Vector(10, 10, 10),
      Vector(10, 10, 10)
    )
    val resultado = objMatriz.sumMatriz(m1, m2)

    assert(resultado == resultadoEsperado, "La suma de matrices no es correcta")
  }

  // Test para la función resta de matrices
  test("Resta de matrices - resultado correcto") {
    val m1 = Vector(
      Vector(10, 10, 10),
      Vector(10, 10, 10),
      Vector(10, 10, 10)
    )
    val m2 = Vector(
      Vector(1, 2, 3),
      Vector(4, 5, 6),
      Vector(7, 8, 9)
    )
    val resultadoEsperado = Vector(
      Vector(9, 8, 7),
      Vector(6, 5, 4),
      Vector(3, 2, 1)
    )
    val resultado = objMatriz.restaMatriz(m1, m2)

    assert(resultado == resultadoEsperado, "La resta de matrices no es correcta")
  }

  // Test para submatriz
  test("Submatriz - resultado correcto") {
    val m = Vector(
      Vector(1, 2, 3, 4),
      Vector(5, 6, 7, 8),
      Vector(9, 10, 11, 12),
      Vector(13, 14, 15, 16)
    )
    val resultadoEsperado = Vector(
      Vector(6, 7),
      Vector(10, 11)
    )
    val resultado = objMatriz.subMatriz(m, 1, 1, 2)

    assert(resultado == resultadoEsperado, "La submatriz no es correcta")
  }

  // Test para la multiplicación recursiva paralela vs secuencial
  test("Multiplicación recursiva paralela vs secuencial - resultados equivalentes") {
    val size = 4
    val m1 = matrizAleatoria(size, 10)
    val m2 = matrizAleatoria(size, 10)

    val resultadoRec = objMatriz.multMatrizRec(m1, m2)
    val resultadoParRec = objMatriz.multMatrizRecPar(m1, m2)

    assert(resultadoRec == resultadoParRec, "La multiplicación recursiva paralela no coincide con la secuencial")
  }

  // Test para Strassen paralelo vs secuencial
  test("Multiplicación Strassen paralelo vs secuencial - resultados equivalentes") {
    val size = 4
    val m1 = matrizAleatoria(size, 10)
    val m2 = matrizAleatoria(size, 10)

    val resultadoStrassen = objMatriz.multStrassen(m1, m2)
    val resultadoStrassenPar = objMatriz.multStrassenPar(m1, m2)

    assert(resultadoStrassen == resultadoStrassenPar, "La multiplicación Strassen paralela no coincide con la secuencial")
  }
}
