package org.ergoplatform.wallet.crypto

import org.ergoplatform.wallet.crypto
import org.ergoplatform.wallet.utils.Generators
import org.scalacheck.Shrink
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class EncryptionSpec
  extends PropSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with Generators {

  implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  property("AES encryption/decryption") {
    forAll(dataGen, passwordGen, encryptionSettingsGen) { (data, pass, settings) =>
      val iv = scorex.utils.Random.randomBytes(16)
      val salt = scorex.utils.Random.randomBytes(32)
      val (encrypted, mac) = crypto.AES.encrypt(data, pass, salt, iv)(settings)
      val decryptedTry = crypto.AES.decrypt(encrypted, pass, salt, iv, mac)(settings)

      decryptedTry shouldBe 'success
      decryptedTry.get shouldEqual data
    }
  }

  property("AES encryption/decryption - failure on corrupted data decryption") {
    forAll(dataGen, passwordGen, encryptionSettingsGen) { (data, pass, settings) =>
      val iv = scorex.utils.Random.randomBytes(16)
      val salt = scorex.utils.Random.randomBytes(32)
      val (encrypted, mac) = crypto.AES.encrypt(data, pass, salt, iv)(settings)
      encrypted(scala.util.Random.nextInt(encrypted.length)) = 0x0
      val decryptedTry = crypto.AES.decrypt(encrypted, pass, salt, iv, mac)(settings)

      decryptedTry shouldBe 'failure
    }
  }

}
