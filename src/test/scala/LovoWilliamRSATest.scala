import org.scalatest.{BeforeAndAfterAll, FunSuite}

class LovoWilliamRSATest extends FunSuite {

  val test = new LovoWilliamRSA

  test("The gcd of 30 and 288 should be 6") {
    assert(test.gcd(30, 288) == 6)
  }

  test("The gcd of 29 and 288 should be 1") {
    assert(test.gcd(288, 29) == 1)
  }

  test("The multiplicative inverse of 29 mod 288 should be 149") {
    assert(test.xgcd(29, 288) == 149)
  }

  test("The multiplicative inverse of 149 mod 288 should be 29") {
    assert(test.xgcd(149, 288) == 29)
  }
}
