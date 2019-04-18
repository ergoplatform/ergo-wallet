package org.ergoplatform.wallet.secrets

import org.bouncycastle.util.BigIntegers
import org.ergoplatform.wallet.crypto.HmacSHA512
import org.ergoplatform.wallet.settings.Constants
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}
import sigmastate.interpreter.CryptoConstants

/**
  * Secret, its chain code and path in key tree.
  * (see: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki)
  */
final class ExtendedPublicKey(val keyBytes: Array[Byte],
                              val chainCode: Array[Byte],
                              val path: DerivationPath,
                              val isNeutered: Boolean = false)
  extends ExtendedKey {

  def decodedUnsafe: ProveDlog = ProveDlog(
    CryptoConstants.dlogGroup.curve.decodePoint(keyBytes).asInstanceOf[CryptoConstants.EcPointType]
  )

  def child(idx: Int): ExtendedPublicKey = ExtendedPublicKey.deriveChildPublicKey(this, idx)
}

object ExtendedPublicKey {

  def deriveChildPublicKey(parentKey: ExtendedPublicKey, idx: Int): ExtendedPublicKey = {
    require(!Index.isHardened(idx), "Hardened public keys derivation is not supported")
    val (childKeyProto, childChainCode) = HmacSHA512
      .hash(parentKey.chainCode, parentKey.keyBytes ++ Index.serializeIndex(idx))
      .splitAt(Constants.KeyLen)
    val childKeyProtoDecoded = BigIntegers.fromUnsignedByteArray(childKeyProto)
    val childKey = DLogProverInput(childKeyProtoDecoded).publicImage.value.add(parentKey.decodedUnsafe.value)
    if (childKeyProtoDecoded.compareTo(CryptoConstants.groupOrder) >= 0 || childKey.isInfinity)
      deriveChildPublicKey(parentKey, idx + 1)
    else
      new ExtendedPublicKey(childKey.getEncoded(true), childChainCode, parentKey.path.extended(idx))
  }

}
