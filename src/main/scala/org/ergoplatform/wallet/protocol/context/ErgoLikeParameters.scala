package org.ergoplatform.wallet.protocol.context

trait ErgoLikeParameters {

  def storageFeeFactor: Int

  def minValuePerByte: Int

  def maxBlockSize: Int

  def tokenAccessCost: Int

  def inputCost: Int

  def dataInputCost: Int

  def outputCost: Int

  def maxBlockCost: Long

  def softForkStartingHeight: Option[Int]

  def softForkVotesCollected: Option[Int]

  def blockVersion: Byte

}
