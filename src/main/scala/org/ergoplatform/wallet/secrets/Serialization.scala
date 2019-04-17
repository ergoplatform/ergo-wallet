package org.ergoplatform.wallet.secrets

object Serialization {

  val UIntMaxValue = 4294967295L

  def ser32(i: Long): Array[Byte] = {
    require(i >= 0 && i <= UIntMaxValue, "Unsigned integer out of range")
    Array((i >> 24).toByte, (i >> 16).toByte, (i >> 8).toByte, i.toByte)
  }

}
