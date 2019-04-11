package org.ergoplatform.wallet.crypto

object Utils {

  def padPKCS5(input: Array[Byte], size: Int = 16): Array[Byte] = {
    val padByte: Int = size - (input.length % size)
    input ++ Array.fill[Byte](padByte)(padByte.toByte)
  }

  def unpadPKCS5(input: Array[Byte]): Array[Byte] = {
    val padByte = input.last
    val length = input.length
    require(padByte <= length, "The input was shorter than the padding byte indicates")
    require(
      input.takeRight(padByte).containsSlice(Array.fill[Byte](padByte)(padByte)),
      "Padding format is not as being expected"
    )
    input.take(length - padByte)
  }

}
