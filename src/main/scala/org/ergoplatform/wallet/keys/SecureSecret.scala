package org.ergoplatform.wallet.keys

import org.bouncycastle.util.BigIntegers
import sigmastate.basics.DLogProtocol.DLogProverInput

final class SecureSecret(val secretBytes: Array[Byte]) {
  def secret: DLogProverInput = DLogProverInput(BigIntegers.fromUnsignedByteArray(secretBytes))
  def zeroSecret(): Unit = secretBytes.zipWithIndex.foreach { case (_, idx) =>
    secretBytes(idx) = 0x0
  }
}
