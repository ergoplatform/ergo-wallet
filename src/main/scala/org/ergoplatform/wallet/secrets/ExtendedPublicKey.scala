package org.ergoplatform.wallet.secrets

import org.bouncycastle.util.BigIntegers
import sigmastate.basics.DLogProtocol.DLogProverInput

/**
  * Wraps secret as mutable array allowing to destroy it in the memory.
  */
final class ExtendedPublicKey(val secretBytes: Array[Byte]) {
  def secret: DLogProverInput = DLogProverInput(BigIntegers.fromUnsignedByteArray(secretBytes))
  def isErased: Boolean = secretBytes.forall(_ == 0x0)
  def zeroSecret(): Unit = secretBytes.zipWithIndex.foreach { case (_, idx) =>
    secretBytes(idx) = 0x0
  }
}
