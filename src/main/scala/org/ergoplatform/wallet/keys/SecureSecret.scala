package org.ergoplatform.wallet.keys

import java.math.BigInteger

import sigmastate.basics.DLogProtocol.DLogProverInput

final class SecureSecret(val secretBytes: Array[Byte]) {
  def secret: DLogProverInput = DLogProverInput(new BigInteger(secretBytes))
  def zeroSecret(): Unit = secretBytes.zipWithIndex.foreach { case (_, idx) =>
    secretBytes(idx) = 0x0
  }
}
