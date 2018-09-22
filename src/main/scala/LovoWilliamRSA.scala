import scala.annotation.tailrec
import scala.util.Random

/**
  * Project 1 assignment. The class can handle the different processes of the RSA encryption method,
  * including the ability to process the gcd, xgcd, key generation, encrypting, and decrypting.
  *
  * The main class creates different examples of each step.
  * @author William Lovo
  */
class LovoWilliamRSA {
  /**
    * Finds the greatest common divisor (gcd) of the two numbers given.
    * @param inE The first given number
    * @param inZ The second given number
    * @return the gcd
    */
  @tailrec
  final def gcd(inE: Int, inZ: Int): Int = inZ match {
    case 0 => inE
    case _ => gcd(inZ, inE % inZ)
  }

  /**
    * Test class, used to verify the gcd function.
    */
  def testGcd(): Unit = {
    val test: LovoWilliamRSA = new LovoWilliamRSA
    printf(s"GCD (29, 288) = 0x${test.gcd(29, 288).toHexString}\n")
    printf(s"GCD (30, 288) = 0x${test.gcd(30, 288).toHexString}\n\n")
  }

  /**
    * Calculates the multiplicative inverse of inE mod inZ using the extended Euclidean method.
    * Uses the recursive helper function.
    * @param inE the e, which should be 1 < e < Z
    * @param inZ the Z
    * @return the multiplicative inverse of inE mod inZ
    */
  def xgcd(inE: Int, inZ: Int): Int = {
    _xgcd(inZ, inE % inZ, Array(0, 1)) match {
      case x if x < 0 => x + inZ
      case x if x >= 0 && x < 2 || x > inZ => -1
      case x => x
    }
  }

  /**
    * Recursive method to calculate the multiplicative inverse of the initial inE mod inZ.
    * @param d1 the first d number
    * @param d2 the second d number
    * @param t the array of the last two t values
    * @return the multiplicative inverse
    */
  @tailrec
  private def _xgcd(d1: Int, d2: Int, t: Array[Int]): Int = {
    if (d2 == 0) t(0)
    else {
      val q: Int = scala.math.floor(d1 / d2).toInt

      val temp: Int = t(0) - t(1) * q
      val new_t: Array[Int] = Array(t(1), temp)

      _xgcd(d2, d1 % d2, new_t)
    }
  }

  /**
    * Test class, used to verify the xgcd function.
    */
  def testXgcd(): Unit = {
    val test: LovoWilliamRSA = new LovoWilliamRSA
    printf(s"29^-1 mod 288 = 0x${test.xgcd(29, 288).toHexString}\n")
    printf(s"149^-1 mod 288 = 0x${test.xgcd(149, 288).toHexString}\n\n")
  }

  /**
    * The key generation function, using the method described in class.
    * @param inP the first number
    * @param inQ the second number
    * @param inE the given e value
    * @return the array of the public and private keys
    */
  def keygen(inP: Int, inQ: Int, inE: Int): Array[Int] = {
    val N: Int = inP * inQ
    val Z: Int = (inP - 1) * (inQ - 1)

    val e: Int = inE match {
      case x if !(x > 1) => gen_new_e(Z)
      case x if x > 1 && x < Z && gcd(x, Z) == 1 => x
      case _ => throw new IllegalArgumentException
    }

    val d: Int = xgcd(e, Z)

    Array(e, N, d)
  }

  /**
    * Helper method to create a new e if needed.
    * @return the new e
    */
  private def gen_new_e: Int => Int = (N: Int) => {
    var new_key: Int = 0
    val start: Int = 2

    val rand: Random.type = Random
    while (gcd(new_key, N) != 1) {
      new_key = rand.nextInt(N - start + 1) + start
    }
    new_key
  }

  /**
    * The encryption function, as describe in class.
    * @param message The message
    * @param inE the given e
    * @param inN the given N
    * @return the encrypted message
    */
  def encrypt(message: Int, inE: Int, inN: Int): Int = {
    mod_exp_one(message, inE, inN)
  }

  /**
    * The decryption function, as described in class.
    * @param ciphertext the encrypted message
    * @param inD the given d
    * @param inN the given N
    * @return the decrypted message
    */
  def decrypt(ciphertext: Int, inD: Int, inN: Int): Int = {
    mod_exp_one(ciphertext, inD, inN)
  }

  /** Calculate c = a^^b mod n
    * Courtesy of Dr. Wang
    * @param a the given number
    * @param b the given exponent
    * @param n the given number
    * @return the modulus value
    */
  private def mod_exp_one(a: Int, b: Int, n: Int): Int = {
    var x: Int = 1
    var w: Int = a
    var y: Int = b
    while (y > 0) {
      val t: Int = y % 2
      y = y / 2
      if (t == 1) {
        val xLong: Long = x * w
        x = (xLong % n).toInt
      }
      val wLong: Long = w * w
      w = (wLong % n).toInt
    }
    x
  }
}

/**
  * The main method. Used to display the test functions and test scenario.
  */
object LovoWilliamRSA {
  def main(args: Array[String]): Unit = {
    val sample: LovoWilliamRSA = new LovoWilliamRSA

    println(s"${"*" * 10} Testing class methods ${"*" * 10}")

    sample.testGcd()
    sample.testXgcd()

    val p: Int = 17
    val q: Int = 19
    val e: Int = 29
    val m: Int = 4

    val keys: Array[Int] = sample.keygen(inP = p, inQ = q, inE = e)
    printf(s"e = ${keys(0)}.\n")
    printf(s"N = ${keys(1)}.\n")
    printf(s"d = 0x${keys(2).toHexString}.\n\n")

    val encrypted_message: Int = sample.encrypt(message = m, inE = keys(0), inN = keys(1))
    printf(s"The encryption of <m=4> is 0x$encrypted_message.\n")

    val decrypted_message: Int = sample.decrypt(encrypted_message, keys(2), keys(1))
    printf(s"The decryption of <c=255> is 0x$decrypted_message.\n")
  }
}

