package org.ergoplatform.wallet.secrets

import java.math.BigInteger
import java.util

import org.bouncycastle.util.BigIntegers
import org.ergoplatform.wallet.crypto.HmacSHA512
import org.ergoplatform.wallet.settings.Constants
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.interpreter.CryptoConstants

/**
  * Secret, its chain code and path in key tree.
  * (see: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki)
  */
final class ExtendedSecretKey(val keyBytes: Array[Byte],
                              val chainCode: Array[Byte],
                              val path: DerivationPath)
  extends ExtendedKey {

  def key: DLogProverInput = DLogProverInput(BigIntegers.fromUnsignedByteArray(keyBytes))

  def child(idx: Int): ExtendedSecretKey = ExtendedSecretKey.deriveChildSecretKey(this, idx)

  def publicKey: ExtendedPublicKey =
    new ExtendedPublicKey(key.publicImage.value.getEncoded(true), chainCode, path.toPublic)

  def isErased: Boolean = keyBytes.forall(_ == 0x0)

  def zeroSecret(): Unit = util.Arrays.fill(keyBytes, 0: Byte)
}

object ExtendedSecretKey {

  def deriveChildSecretKey(parentKey: ExtendedSecretKey, idx: Int): ExtendedSecretKey = {
    val keyCoded: Array[Byte] =
      if (Index.isHardened(idx)) (0x00: Byte) +: parentKey.keyBytes
      else parentKey.key.publicImage.value.getEncoded(true)
    val (childKeyProto, childChainCode) = HmacSHA512
      .hash(parentKey.chainCode, keyCoded ++ Index.serializeIndex(idx))
      .splitAt(Constants.KeyLen)
    val childKeyProtoDecoded = BigIntegers.fromUnsignedByteArray(childKeyProto)
    val childKey = childKeyProtoDecoded
      .add(BigIntegers.fromUnsignedByteArray(parentKey.keyBytes))
      .mod(CryptoConstants.groupOrder)
    if (childKeyProtoDecoded.compareTo(CryptoConstants.groupOrder) >= 0 || childKey.equals(BigInteger.ZERO))
      deriveChildSecretKey(parentKey, idx + 1)
    else
      new ExtendedSecretKey(BigIntegers.asUnsignedByteArray(childKey), childChainCode, parentKey.path.extended(idx))
  }

  def deriveChildPublicKey(parentKey: ExtendedSecretKey, idx: Int): ExtendedPublicKey = {
    val derivedSecret = deriveChildSecretKey(parentKey, idx)
    val derivedPk = derivedSecret.key.publicImage.value.getEncoded(true)
    val derivedPath = derivedSecret.path.copy(publicBranch = true)
    new ExtendedPublicKey(derivedPk, derivedSecret.chainCode, derivedPath)
  }

  def deriveMasterKey(seed: Array[Byte]): ExtendedSecretKey = {
    val (masterKey, chainCode) = HmacSHA512.hash(Constants.BitcoinSeed, seed).splitAt(Constants.KeyLen)
    new ExtendedSecretKey(masterKey, chainCode, DerivationPath.MasterPath)
  }

}
