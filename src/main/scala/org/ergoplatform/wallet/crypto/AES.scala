package org.ergoplatform.wallet.crypto

import javax.crypto.{Cipher, SecretKeyFactory}
import javax.crypto.spec.{IvParameterSpec, PBEKeySpec, SecretKeySpec}
import org.ergoplatform.wallet.settings.EncryptionSettings
import scorex.crypto.hash.Sha256

import scala.util.{Failure, Success, Try}

object AES {

  val ChecksumLen = 4

  val CipherAlgo = "AES"
  val CipherAlgoInstance = s"$CipherAlgo/CTR/PKCS5Padding"

  /**
    * @param data - data to encrypt
    * @param pass - password to derive encryption key from
    * @param salt - sequence of bits, known as a cryptographic salt
    * @param iv   - cipher initialization vector
    * @return     - tuple of resulted ciphertext and encryption key MAC
    */
  def encrypt(data: Array[Byte], pass: String, salt: Array[Byte], iv: Array[Byte])
             (settings: EncryptionSettings): (Array[Byte], Array[Byte]) = {
    require(data.nonEmpty, "Empty data encryption attempt")
    val keySpec = deriveEncryptionKeySpec(pass, salt)(settings)
    val ivSpec = new IvParameterSpec(iv)

    val cipher = Cipher.getInstance(CipherAlgoInstance)
    cipher.init(Cipher.ENCRYPT_MODE, keySpec, ivSpec)

    val checksum = Sha256.hash(data).take(ChecksumLen)
    val padded = padPKCS5(data ++ checksum)
    val ciphertext = cipher.doFinal(padded)
    val keyMac = calcKeyMac(keySpec.getEncoded, ciphertext)

    ciphertext -> keyMac
  }

  /**
    * @param ciphertext - data to decrypt
    * @param pass       - password to derive decryption key from
    * @param salt       - sequence of bits, known as a cryptographic salt
    * @param iv         - cipher initialization vector
    * @param mac        - encryption key MAC
    */
  def decrypt(ciphertext: Array[Byte], pass: String, salt: Array[Byte], iv: Array[Byte], mac: Array[Byte])
             (settings: EncryptionSettings): Try[Array[Byte]] = {
    require(ciphertext.nonEmpty, "Empty ciphertext decryption attempt")
    val keySpec = deriveEncryptionKeySpec(pass, salt)(settings)
    val resultedMac = calcKeyMac(keySpec.getEncoded, ciphertext)

    if (!java.util.Arrays.equals(resultedMac, mac)) Failure(new Exception("Wrong pass"))
    else {
      val ivSpec = new IvParameterSpec(iv)

      val cipher = Cipher.getInstance(CipherAlgoInstance)
      cipher.init(Cipher.DECRYPT_MODE, keySpec, ivSpec)

      val text = cipher.doFinal(ciphertext)
      unpadPKCS5(text).flatMap { unpadded =>
        val (data, checksum) = unpadded.splitAt(unpadded.length - ChecksumLen)
        if (java.util.Arrays.equals(Sha256.hash(data).take(ChecksumLen), checksum)) Success(data)
        else Failure(new Exception("Wrong checksum"))
      }
    }
  }

  private def calcKeyMac(key: Array[Byte], text: Array[Byte]) = Sha256.hash(key.take(16) ++ text)

  private def deriveEncryptionKeySpec(pass: String, salt: Array[Byte])
                                     (settings: EncryptionSettings): SecretKeySpec = {
    val pbeSpec = new PBEKeySpec(pass.toCharArray, salt, settings.c, settings.dkLen)
    val skf = SecretKeyFactory.getInstance(s"PBKDF2With${settings.prf}")
    val encryptionKey = skf.generateSecret(pbeSpec).getEncoded
    new SecretKeySpec(encryptionKey, CipherAlgo)
  }

}
