package org.ergoplatform.wallet.settings

/**
  * Encryption parameters
  * @param prf   - Pseudo-random function with output of length `dkLen` (PBKDF2 param)
  * @param c     - Number of PBKDF2 iterations (PBKDF2 param)
  * @param dkLen - desired bit-length of the derived key (PBKDF2 param)
  */
final case class EncryptionSettings(prf: String, c: Int, dkLen: Int)
