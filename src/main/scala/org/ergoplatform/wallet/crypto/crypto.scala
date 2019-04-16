package org.ergoplatform.wallet

import scala.util.{Failure, Success, Try}

package object crypto {

  def padPKCS5(input: Array[Byte], size: Int = 16): Array[Byte] = {
    val padByte: Int = size - (input.length % size)
    input ++ Array.fill[Byte](padByte)(padByte.toByte)
  }

  def unpadPKCS5(input: Array[Byte]): Try[Array[Byte]] = {
    val padByte = input.last
    val length = input.length

    if (padByte > length) Failure(new Exception("The input was shorter than the padding byte indicates"))
    else if (!input.takeRight(padByte).containsSlice(Array.fill[Byte](padByte)(padByte)))
      Failure(new Exception("Padding format is not as expected"))
    else Success(input.take(length - padByte))
  }

}
