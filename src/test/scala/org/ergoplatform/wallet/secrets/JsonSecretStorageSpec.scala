package org.ergoplatform.wallet.secrets

import org.ergoplatform.wallet.settings.WalletSettings
import org.ergoplatform.wallet.utils.{FileUtils, Generators}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class JsonSecretStorageSpec
  extends PropSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with Generators
    with FileUtils {

  property("initialization and unlock") {
    forAll(entropyGen, passwordGen, encryptionSettingsGen) { (seed, pass, cryptoSettings) =>
      val dir = createTempDir
      val settings = WalletSettings(dir.getAbsolutePath, cryptoSettings)
      val storage = JsonSecretStorage.init(seed, pass)(settings)

      storage.isLocked shouldBe true

      val unlockTry = storage.unlock(IndexedSeq(1, 2, 3), pass)

      unlockTry shouldBe 'success

      storage.isLocked shouldBe false
    }
  }

  property("secrets erasure on lock") {
    forAll(entropyGen, passwordGen, encryptionSettingsGen) { (seed, pass, cryptoSettings) =>
      val dir = createTempDir
      val settings = WalletSettings(dir.getAbsolutePath, cryptoSettings)
      val storage = JsonSecretStorage.init(seed, pass)(settings)

      storage.unlock(IndexedSeq(1, 2, 3), pass)

      val secrets = storage.secrets.values

      secrets.nonEmpty shouldBe true

      storage.lock()

      secrets.forall(_.isErased) shouldBe true

      storage.secrets.isEmpty shouldBe true
    }
  }

}
