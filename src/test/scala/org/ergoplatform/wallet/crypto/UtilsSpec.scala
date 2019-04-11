package org.ergoplatform.wallet.crypto

import org.ergoplatform.wallet.EncryptionSettings
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class UtilsSpec extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val passwordGen: Gen[String] = Gen.nonEmptyListOf(Gen.alphaNumChar).map(_.toString)
  val dataGen: Gen[Array[Byte]] = Gen.nonEmptyListOf(Gen.posNum[Byte]).map(_.toArray)

  val settingsGen: Gen[EncryptionSettings] = for {
    prf <- Gen.oneOf(Seq("HmacSHA1", "HmacSHA256", "HmacSHA512"))
    c <- Gen.posNum[Int]
  } yield EncryptionSettings(prf, c, 256)

  property("AES encryption/decryption") {
    forAll(dataGen, passwordGen, settingsGen) { (data, pass, settings) =>
      val iv = scorex.utils.Random.randomBytes(16)
      val salt = scorex.utils.Random.randomBytes(32)
      val encrypted = Utils.AES.encrypt(data, pass, salt, iv)(settings)
      val decryptedTry = Utils.AES.decrypt(encrypted, pass, salt, iv)(settings)

      decryptedTry shouldBe 'success
      decryptedTry.get shouldEqual data
    }
  }

}
