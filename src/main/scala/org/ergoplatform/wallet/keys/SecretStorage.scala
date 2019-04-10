package org.ergoplatform.wallet.keys

import java.io.File

import sigmastate.basics.DLogProtocol.DLogProverInput

import scala.util.Try

trait SecretStorage {

  val keyDir: File

  def secrets: IndexedSeq[DLogProverInput]

  def unlock(secretsIndices: IndexedSeq[Int], passphrase: String): Try[Unit]

  def lock(): Unit

}
