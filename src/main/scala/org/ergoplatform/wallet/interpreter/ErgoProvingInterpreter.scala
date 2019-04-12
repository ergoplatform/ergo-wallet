package org.ergoplatform.wallet.interpreter

import java.util

import org.ergoplatform._
import org.ergoplatform.wallet.protocol.context.{ErgoContext, ErgoLikeParameters, ErgoLikeStateContext, TransactionContext}
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}
import sigmastate.eval.{IRContext, RuntimeIRContext}
import sigmastate.interpreter.{ContextExtension, ProverInterpreter}

import scala.util.{Failure, Success, Try}

/**
  * A class which is holding secrets and signing transactions.
  *
  * Currently it just generates some number of secrets (the number is provided via "dlogSecretsNumber" setting in the
  * "wallet" section) from a seed and sign a transaction (against input boxes to spend and
  * blockchain state) by using the secrets (no additional inputs, e.g. hash function preimages required in scripts,
  * are supported. Here, signing a transaction means spending proofs generation for all of its input boxes.
  *
  * @param seed    - a secret seed value
  * @param maxCost - max cost of all the transaction input scripts combined (the prover refuses to sign a transaction
  *                if the total cost exceeds the limit)
  */
class ErgoProvingInterpreter(val secrets: IndexedSeq[DLogProverInput], params: ErgoLikeParameters)
                            (implicit IR: IRContext)
  extends ErgoInterpreter(params) with ProverInterpreter {

  val pubKeys: IndexedSeq[ProveDlog] = secrets.map(_.publicImage)

  /** Require `unsignedTx` and `boxesToSpend` have the same boxIds in the same order */
  def sign(unsignedTx: UnsignedErgoLikeTransaction,
           boxesToSpend: IndexedSeq[ErgoBox],
           dataBoxes: IndexedSeq[ErgoBox],
           stateContext: ErgoLikeStateContext): Try[ErgoLikeTransactionTemplate[Input]] = Try {

    require(unsignedTx.inputs.length == boxesToSpend.length, "Not enough boxes to spend")
    require(unsignedTx.dataInputs.length == dataBoxes.length, "Not enough data boxes")

    boxesToSpend.zipWithIndex
      .foldLeft(Try(IndexedSeq[Input]() -> 0L)) { case (inputsCostTry, (inputBox, boxIdx)) =>
        val unsignedInput = unsignedTx.inputs(boxIdx)
        require(util.Arrays.equals(unsignedInput.boxId, inputBox.id))

        val transactionContext = TransactionContext(boxesToSpend, dataBoxes, unsignedTx, boxIdx.toShort)

        inputsCostTry.flatMap { case (ins, totalCost) =>
          val context = new ErgoContext(stateContext, transactionContext, ContextExtension.empty)

          prove(inputBox.ergoTree, context, unsignedTx.messageToSign).flatMap { proverResult =>
            val newTC = totalCost + proverResult.cost
            if (newTC > maxCost) {
              Failure(new Exception(s"Computational cost of transaction $unsignedTx exceeds limit $maxCost"))
            } else {
              Success((Input(unsignedInput.boxId, proverResult) +: ins) -> newTC)
            }
          }
        }
      }
      .map { case (inputs, _) =>
        new ErgoLikeTransaction(inputs.reverse, unsignedTx.dataInputs, unsignedTx.outputCandidates)
      }
  }.flatten

}


object ErgoProvingInterpreter {

  def apply(secrets: IndexedSeq[DLogProverInput], params: ErgoLikeParameters): ErgoProvingInterpreter =
    new ErgoProvingInterpreter(secrets, params)(new RuntimeIRContext)
}
