package org.ergoplatform.wallet.protocol.context

import org.ergoplatform.wallet.protocol.Constants
import org.ergoplatform.{ErgoLikeContext, ErgoLikeTransactionTemplate, UnsignedInput}
import scorex.crypto.authds.ADDigest
import sigmastate.interpreter.ContextExtension
import sigmastate.{AvlTreeData, AvlTreeFlags}

class ErgoContext(val stateContext: ErgoLikeStateContext,
                  transactionContext: TransactionContext,
                  override val extension: ContextExtension = ContextExtension(Map()))
  extends ErgoLikeContext(stateContext.currentHeight,
    ErgoContext.stateTreeFromDigest(stateContext.previousStateDigest),
    stateContext.lastBlockMinerPk,
    stateContext.sigmaLastHeaders,
    stateContext.sigmaPreHeader,
    transactionContext.dataBoxes,
    transactionContext.boxesToSpend,
    transactionContext.spendingTransaction,
    transactionContext.self,
    extension
  ) {

  override def withExtension(newExtension: ContextExtension): ErgoContext =
    new ErgoContext(stateContext, transactionContext, newExtension)

  override def withTransaction(newSpendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput]): ErgoContext =
    new ErgoContext(stateContext, transactionContext.copy(spendingTransaction = newSpendingTransaction), extension)
}

object ErgoContext {
  def stateTreeFromDigest(digest: ADDigest): AvlTreeData = {
    val flags = AvlTreeFlags(insertAllowed = true, updateAllowed = true, removeAllowed = true)
    AvlTreeData(digest, flags, Constants.HashLength)
  }
}
