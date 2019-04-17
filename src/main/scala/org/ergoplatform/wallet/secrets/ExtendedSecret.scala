package org.ergoplatform.wallet.secrets

import java.util

import org.bouncycastle.util.BigIntegers
import org.ergoplatform.wallet.crypto.HmacSHA512
import org.ergoplatform.wallet.settings.Constants
import sigmastate.basics.DLogProtocol.DLogProverInput

/**
  * Wraps secret as mutable array allowing to destroy it in the memory.
  */
final class ExtendedSecret(val secretBytes: Array[Byte], chainCode: Array[Byte]) {
  def secret: DLogProverInput = DLogProverInput(BigIntegers.fromUnsignedByteArray(secretBytes))
  def isErased: Boolean = secretBytes.forall(_ == 0x0)
  def zeroSecret(): Unit = util.Arrays.fill(secretBytes, 0: Byte)
}

object ExtendedSecret {

  def fromSeed(seed: Array[Byte]): ExtendedSecret = {
    val (masterKey, chainCode) = HmacSHA512.hash(Constants.BitcoinSeed, seed).splitAt(Constants.KeyLen)
    new ExtendedSecret(masterKey, chainCode)
  }

}
