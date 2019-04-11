package org.ergoplatform.wallet.crypto

import javax.crypto.{Cipher, SecretKeyFactory}
import javax.crypto.spec.{IvParameterSpec, PBEKeySpec, SecretKeySpec}
import org.ergoplatform.wallet.EncryptionSettings
import scorex.crypto.hash.Sha256

import scala.util.{Failure, Success, Try}

object Utils {

  object AES {

    val ChecksumLen = 4

    def encrypt(data: Array[Byte], pass: String, salt: Array[Byte], iv: Array[Byte])
               (settings: EncryptionSettings): Array[Byte] = {
      require(data.nonEmpty, "Empty data encryption attempt")
      val keySpec = deriveEncryptionKeySpec(pass, salt)(settings)
      val ivSpec = new IvParameterSpec(iv)

      val cipher = Cipher.getInstance("AES/CTR/PKCS5Padding")
      cipher.init(Cipher.ENCRYPT_MODE, keySpec, ivSpec)

      val checksum = Sha256.hash(data).take(ChecksumLen)
      val padded = padPKCS5(data ++ checksum)
      cipher.doFinal(padded)
    }

    def decrypt(cipherText: Array[Byte], pass: String, salt: Array[Byte], iv: Array[Byte])
               (settings: EncryptionSettings): Try[Array[Byte]] = {
      require(cipherText.nonEmpty, "Empty ciphertext decryption attempt")
      val keySpec = deriveEncryptionKeySpec(pass, salt)(settings)
      val ivSpec = new IvParameterSpec(iv)

      val cipher = Cipher.getInstance("AES/CTR/PKCS5Padding")
      cipher.init(Cipher.DECRYPT_MODE, keySpec, ivSpec)

      val decrypted = cipher.doFinal(cipherText)
      val unpaded = Utils.unpadPKCS5(decrypted)
      val (data, checksum) = unpaded.splitAt(unpaded.length - ChecksumLen)
      if (java.util.Arrays.equals(Sha256.hash(data).take(ChecksumLen), checksum)) Success(data)
      else Failure(new Exception("Data corrupted"))
    }

    private def deriveEncryptionKeySpec(pass: String, salt: Array[Byte])
                                       (settings: EncryptionSettings): SecretKeySpec = {
      val pbeSpec = new PBEKeySpec(pass.toCharArray, salt, settings.c, settings.dkLen)
      val skf = SecretKeyFactory.getInstance(s"PBKDF2With${settings.prf}")
      val encryptionKey = skf.generateSecret(pbeSpec).getEncoded
      new SecretKeySpec(encryptionKey, "AES")
    }

  }

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
