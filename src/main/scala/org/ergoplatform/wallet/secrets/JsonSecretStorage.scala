package org.ergoplatform.wallet.secrets

import java.io.{File, PrintWriter}
import java.util.UUID

import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.ergoplatform.wallet.crypto
import org.ergoplatform.wallet.settings.{EncryptionSettings, WalletSettings}
import scorex.util.encode.Base16

import scala.util.Try

/**
  * Secret storage backend. Stores encrypted seed in json file in the file system.
  * Responsible for managing access to the secrets.
  */
final class JsonSecretStorage(val secretFile: File, encryptionSettings: EncryptionSettings)
  extends SecretStorage {

  private var unlockedSecrets: Map[Int, SecureSecret] = Map.empty

  override def isLocked: Boolean = unlockedSecrets.isEmpty

  override def secrets: Map[Int, SecureSecret] =
    unlockedSecrets.map { case (i, x) => i -> x }

  override def unlock(secretsIndices: IndexedSeq[Int], pass: String): Try[Unit] = {
    val secretFileRaw = scala.io.Source.fromFile(secretFile, "UTF-8").getLines().mkString
    decode[EncryptedSecret](secretFileRaw)
      .map { encryptedSecret =>
        Base16.decode(encryptedSecret.cipherText)
          .flatMap { txt =>
            Base16.decode(encryptedSecret.salt)
              .flatMap(salt => Base16.decode(encryptedSecret.iv).map((txt, salt, _)))
          }
          .flatMap { case (cipherText, salt, iv) =>
            crypto.AES.decrypt(cipherText, pass, salt, iv)(encryptionSettings)
          }
      }
      .toTry
      .flatten
      .map { seed =>
        secretsIndices.foreach { idx =>
          unlockedSecrets += idx -> new SecureSecret(secretFromSeed(idx, seed))
        }
      }
  }

  override def lock(): Unit = {
    unlockedSecrets.values.foreach(_.zeroSecret())
    unlockedSecrets = Map.empty
  }

}

object JsonSecretStorage {

  /**
    * Initializes storage instance with new wallet file encrypted with the given `pass`.
    */
  def init(seed: Array[Byte], pass: String)(settings: WalletSettings): JsonSecretStorage = {
    val iv = scorex.utils.Random.randomBytes(16)
    val salt = scorex.utils.Random.randomBytes(32)
    val encrypted = crypto.AES.encrypt(seed, pass, salt, iv)(settings.encryption)
    val encryptedSecret = EncryptedSecret(encrypted, salt, iv, settings.encryption)
    val uuid = UUID.nameUUIDFromBytes(encrypted)
    new File(settings.secretDir).mkdirs()
    val file = new File(s"${settings.secretDir}/$uuid.json")
    val outWriter = new PrintWriter(file)
    val jsonRaw = encryptedSecret.asJson.noSpaces

    outWriter.write(jsonRaw)
    outWriter.close()

    new JsonSecretStorage(file, settings.encryption)
  }

}
