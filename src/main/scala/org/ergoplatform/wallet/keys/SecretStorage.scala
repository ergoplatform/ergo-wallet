package org.ergoplatform.wallet.keys

import java.io.File

import sigmastate.basics.DLogProtocol

import scala.util.Try

trait SecretStorage {

  val secretFile: File

  def secrets: Map[Int, DLogProtocol.DLogProverInput]

  def unlock(secretsIndices: IndexedSeq[Int], passphrase: String): Try[Unit]

  def lock(): Unit

}
