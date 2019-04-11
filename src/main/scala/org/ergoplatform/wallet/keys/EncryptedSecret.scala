package org.ergoplatform.wallet.keys

final case class EncryptedSecret(cipherText: String, salt: String, iv: String)
