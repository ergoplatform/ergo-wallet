package org.ergoplatform.wallet.secrets

import java.math.BigInteger
import java.util

import org.bouncycastle.util.BigIntegers
import org.ergoplatform.wallet.crypto.HmacSHA512
import org.ergoplatform.wallet.settings.Constants
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.interpreter.CryptoConstants

/**
  * Wraps the secret and its chain code (see: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki).
  */
final class ExtendedSecretKey(val keyBytes: Array[Byte], val chainCode: Array[Byte], val path: DerivationPath) {

  def key: DLogProverInput = DLogProverInput(BigIntegers.fromUnsignedByteArray(keyBytes))

  def child(idx: Int): ExtendedSecretKey = ExtendedSecretKey.deriveChildSecretKey(this, idx)

  def derive(upPath: DerivationPath): ExtendedSecretKey = {
    require(
      upPath.decodedPath.take(path.depth).zip(path.decodedPath).forall { case (x1, x2) => x1 == x2 }
        && upPath.publicBranch == path.publicBranch,
      s"Incompatible paths: $upPath, $path"
    )
    upPath.decodedPath.drop(path.depth).foldLeft(this)(_ child _)
  }

  def isErased: Boolean = keyBytes.forall(_ == 0x0)

  def zeroSecret(): Unit = util.Arrays.fill(keyBytes, 0: Byte)
}

object ExtendedSecretKey {

  def deriveChildSecretKey(parentKey: ExtendedSecretKey, idx: Int): ExtendedSecretKey = {
    val keyCoded: Array[Byte] =
      if (Index.isHardened(idx)) (0x00: Byte) +: parentKey.keyBytes
      else parentKey.key.publicImage.value.getEncoded(true)
    val (childKeyProto, childChainCode) =
      HmacSHA512.hash(parentKey.chainCode, keyCoded ++ Index.serializeIndex(idx)).splitAt(Constants.KeyLen)
    val childKeyProtoDecoded = BigIntegers.fromUnsignedByteArray(childKeyProto)
    val nextKey =
      childKeyProtoDecoded.add(BigIntegers.fromUnsignedByteArray(parentKey.keyBytes).mod(CryptoConstants.groupOrder))
    if (childKeyProtoDecoded.compareTo(CryptoConstants.groupOrder) >= 0 || nextKey.equals(BigInteger.ZERO))
      deriveChildSecretKey(parentKey, idx + 1)
    else
      new ExtendedSecretKey(BigIntegers.asUnsignedByteArray(nextKey), childChainCode, parentKey.path.extended(idx))
  }

  def fromSeed(seed: Array[Byte]): ExtendedSecretKey = {
    val (masterKey, chainCode) = HmacSHA512.hash(Constants.BitcoinSeed, seed).splitAt(Constants.KeyLen)
    new ExtendedSecretKey(masterKey, chainCode, DerivationPath.masterPath)
  }

}
