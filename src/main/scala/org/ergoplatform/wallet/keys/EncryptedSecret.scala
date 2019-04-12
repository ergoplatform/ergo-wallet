package org.ergoplatform.wallet.keys

import org.ergoplatform.wallet.settings.EncryptionSettings
import scorex.util.encode.Base16

final case class EncryptedSecret(cipherText: String, salt: String, iv: String,
                                 cipherParams: EncryptionSettings)

object EncryptedSecret {
  def apply(cipherText: Array[Byte], salt: Array[Byte], iv: Array[Byte],
            cipherParams: EncryptionSettings): EncryptedSecret =
    new EncryptedSecret(Base16.encode(cipherText), Base16.encode(salt), Base16.encode(iv), cipherParams)
}
