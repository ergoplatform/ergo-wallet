package org.ergoplatform.wallet.keys

import java.io.File

import io.circe.generic.auto._
import io.circe.parser._
import org.ergoplatform.wallet.crypto
import org.ergoplatform.wallet.settings.EncryptionSettings
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import sigmastate.basics.DLogProtocol

import scala.util.Try

final class JsonSecretStorage(val secretFile: File, encryptionSettings: EncryptionSettings)
  extends SecretStorage {

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
          .flatMap { case (cipherText, salt, iv) =>
            crypto.AES.decrypt(cipherText, pass, salt, iv)(encryptionSettings)
          }
      }
      .toTry
      .flatten
      .map { seed =>
        secretsIndices.foreach { idx =>
          unlockedSecrets += idx -> secretFromSeed(idx, seed)
        }
      }
  }

  override def lock(): Unit = {
    unlockedSecrets.values.foreach(_.zeroSecret())
    unlockedSecrets = Map.empty
  }

  private def secretFromSeed(idx: Int, seed: Array[Byte]): SecureSecret =
    new SecureSecret(Blake2b256.hash(idx + Base16.encode(seed)))

}
