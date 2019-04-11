package org.ergoplatform.wallet

final case class EncryptionSettings(prf: String, c: Int, dkLen: Int)

final case class WalletSettings(secretDir: String, encryption: EncryptionSettings)
