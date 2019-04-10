package org.ergoplatform.wallet.keys

import java.io.File

import sigmastate.basics.DLogProtocol

import scala.util.Try

final class JsonSecretStorage(val keyDir: File) extends SecretStorage {

  private var unlockedSecrets: IndexedSeq[SecureSecret] = IndexedSeq.empty

  override def secrets: IndexedSeq[DLogProtocol.DLogProverInput] = unlockedSecrets.map(_.secret)

  override def unlock(secretsIndices: IndexedSeq[Int], passphrase: String): Try[Unit] = ???

  override def lock(): Unit = {
    unlockedSecrets.foreach(_.zeroSecret())
    unlockedSecrets = IndexedSeq.empty
  }

}
