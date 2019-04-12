package org.ergoplatform.wallet.protocol.context

case class ErgoLikeParameters(storageFeeFactor: Int,
                              minValuePerByte: Int,
                              maxBlockSize: Int,
                              tokenAccessCost: Int,
                              inputCost: Int,
                              dataInputCost: Int,
                              outputCost: Int,
                              maxBlockCost: Long,
                              softForkStartingHeight: Option[Int],
                              softForkVotesCollected: Option[Int],
                              blockVersion: Byte)
