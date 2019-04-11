package org.ergoplatform.wallet.settings

final case class EncryptionSettings(prf: String, c: Int, dkLen: Int)
