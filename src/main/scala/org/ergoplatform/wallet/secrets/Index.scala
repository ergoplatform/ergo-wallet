package org.ergoplatform.wallet.secrets

object Index {

  val HardRangeStart = 0x80000000

  def hardIndex(i: Int): Int = i | HardRangeStart

  def isHardened(i: Int): Boolean = (i & HardRangeStart) != 0

  def serializeIndex(i: Int): Array[Byte] = Array(
    (i >> 24).toByte,
    (i >> 16).toByte,
    (i >> 8).toByte,
    i.toByte
  )

}
