package org.ergoplatform.wallet.utils

import org.ergoplatform.wallet.mnemonic.{Mnemonic, WordList}
import org.ergoplatform.wallet.secrets.{DerivationPath, Index}
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

  val derivationPathGen: Gen[DerivationPath] = for {
    isPublic <- Gen.oneOf(Seq(true, false))
    indices <- Gen.listOf(Gen.oneOf(Seq(true, false))
      .flatMap(x => Gen.posNum[Int].map(i => if (x) Index.hardIndex(i) else i)))
  } yield DerivationPath(0 +: indices, isPublic)

}
