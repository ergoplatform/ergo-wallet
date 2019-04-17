package org.ergoplatform.wallet.secrets

import java.io.File

import sigmastate.basics.DLogProtocol

import scala.util.Try

trait SecretStorage {

  val secretFile: File

  def isLocked: Boolean

  def secrets: Map[Int, ExtendedSecret]

  def unlock(secretsIndices: IndexedSeq[Int], pass: String): Try[Unit]

  def lock(): Unit

}
