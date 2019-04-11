package org.ergoplatform.wallet.keys

import java.io.File

import io.circe.generic.auto._
import io.circe.parser._
import javax.crypto.{Cipher, SecretKeyFactory}
import javax.crypto.spec.{IvParameterSpec, PBEKeySpec, SecretKeySpec}
import org.ergoplatform.wallet.EncryptionSettings
import org.ergoplatform.wallet.crypto.Utils
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import sigmastate.basics.DLogProtocol

import scala.util.Try

final class JsonSecretStorage(val secretFile: File, encryptionSettings: EncryptionSettings) extends SecretStorage {

  private var unlockedSecrets: Map[Int, SecureSecret] = Map.empty

  override def secrets: Map[Int, DLogProtocol.DLogProverInput] =
    unlockedSecrets.map { case (i, x) => i -> x.secret }

  override def unlock(secretsIndices: IndexedSeq[Int], pass: String): Try[Unit] = {
    val secretFileRaw = scala.io.Source.fromFile(secretFile, "UTF-8").getLines().mkString
    decode[EncryptedSecret](secretFileRaw)
      .map { encryptedSecret =>
        Base16.decode(encryptedSecret.cipherText)
          .flatMap { txt =>
            Base16.decode(encryptedSecret.salt)
              .flatMap(salt => Base16.decode(encryptedSecret.iv).map((txt, salt, _)))
          }
          .foreach { case (cipherText, salt, iv) =>
            val pbeSpec = new PBEKeySpec(pass.toCharArray, salt, encryptionSettings.c, encryptionSettings.dkLen)
            val skf = SecretKeyFactory.getInstance(s"PBKDF2With${encryptionSettings.prf}")
            val encryptionKey = skf.generateSecret(pbeSpec).getEncoded
            val keySpec = new SecretKeySpec(encryptionKey, "AES")
            val ivSpec = new IvParameterSpec(iv)
            val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
            cipher.init(Cipher.DECRYPT_MODE, keySpec, ivSpec)
            val seed = Utils.unpadPKCS5(cipher.doFinal(cipherText))
            secretsIndices.foreach { idx =>
              unlockedSecrets += idx -> secretFromSeed(idx, seed)
            }
          }
      }
      .toTry
  }

  override def lock(): Unit = {
    unlockedSecrets.values.foreach(_.zeroSecret())
    unlockedSecrets = Map.empty
  }

  private def secretFromSeed(idx: Int, seed: Array[Byte]): SecureSecret =
    new SecureSecret(Blake2b256.hash(idx + Base16.encode(seed)))

}
