package org.ergoplatform.wallet

import javax.crypto.{Cipher, SecretKeyFactory}
import javax.crypto.spec.{IvParameterSpec, PBEKeySpec, SecretKeySpec}
import org.ergoplatform.wallet.settings.EncryptionSettings
import scorex.crypto.hash.Sha256

import scala.util.{Failure, Success, Try}

package object crypto {

  object AES {

    val ChecksumLen = 4

    val CipherAlgo = "AES"
    val CipherAlgoInstance = s"$CipherAlgo/CTR/PKCS5Padding"

    /**
      * @param data - data to encrypt
      * @param pass - password to derive encryption key from
      * @param salt - sequence of bits, known as a cryptographic salt
      * @param iv   - cipher initialization vector
      */
    def encrypt(data: Array[Byte], pass: String, salt: Array[Byte], iv: Array[Byte])
               (settings: EncryptionSettings): Array[Byte] = {
      require(data.nonEmpty, "Empty data encryption attempt")
      val keySpec = deriveEncryptionKeySpec(pass, salt)(settings)
      val ivSpec = new IvParameterSpec(iv)

      val cipher = Cipher.getInstance(CipherAlgoInstance)
      cipher.init(Cipher.ENCRYPT_MODE, keySpec, ivSpec)

      val checksum = Sha256.hash(data).take(ChecksumLen)
      val padded = padPKCS5(data ++ checksum)
      cipher.doFinal(padded)
    }

    /**
      * @param cipherText - data to decrypt
      * @param pass       - password to derive decryption key from
      * @param salt       - sequence of bits, known as a cryptographic salt
      * @param iv         - cipher initialization vector
      */
    def decrypt(cipherText: Array[Byte], pass: String, salt: Array[Byte], iv: Array[Byte])
               (settings: EncryptionSettings): Try[Array[Byte]] = {
      require(cipherText.nonEmpty, "Empty ciphertext decryption attempt")
      val keySpec = deriveEncryptionKeySpec(pass, salt)(settings)
      val ivSpec = new IvParameterSpec(iv)

      val cipher = Cipher.getInstance(CipherAlgoInstance)
      cipher.init(Cipher.DECRYPT_MODE, keySpec, ivSpec)

      val decrypted = cipher.doFinal(cipherText)
      unpadPKCS5(decrypted).flatMap { unpaded =>
        val (data, checksum) = unpaded.splitAt(unpaded.length - ChecksumLen)
        if (java.util.Arrays.equals(Sha256.hash(data).take(ChecksumLen), checksum)) Success(data)
        else Failure(new Exception("Data corrupted"))
      }
    }

    private def deriveEncryptionKeySpec(pass: String, salt: Array[Byte])
                                       (settings: EncryptionSettings): SecretKeySpec = {
      val pbeSpec = new PBEKeySpec(pass.toCharArray, salt, settings.c, settings.dkLen)
      val skf = SecretKeyFactory.getInstance(s"PBKDF2With${settings.prf}")
      val encryptionKey = skf.generateSecret(pbeSpec).getEncoded
      new SecretKeySpec(encryptionKey, CipherAlgo)
    }

  }

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
