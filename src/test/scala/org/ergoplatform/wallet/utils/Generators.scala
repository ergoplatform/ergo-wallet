package org.ergoplatform.wallet.utils

import org.ergoplatform.wallet.mnemonic.{Mnemonic, WordList}
import org.ergoplatform.wallet.settings.EncryptionSettings
import org.scalacheck.Gen

trait Generators {

  val passwordGen: Gen[String] = Gen.nonEmptyListOf(Gen.alphaNumChar).map(_.toString)
  val dataGen: Gen[Array[Byte]] = Gen.nonEmptyListOf(Gen.posNum[Byte]).map(_.toArray)

  val encryptionSettingsGen: Gen[EncryptionSettings] = for {
    prf <- Gen.oneOf(Seq("HmacSHA1", "HmacSHA256", "HmacSHA512"))
    c <- Gen.posNum[Int]
  } yield EncryptionSettings(prf, c, 256)

  val mnemonicGen: Gen[Mnemonic] = for {
    lang <- Gen.oneOf(WordList.AvailableLanguages)
    strength <- Gen.oneOf(Mnemonic.AllowedStrengths)
  } yield new Mnemonic(lang, strength)

  val entropyGen: Gen[Array[Byte]] = Gen.oneOf(Mnemonic.AllowedEntropyLengths).map(scorex.utils.Random.randomBytes)

}
