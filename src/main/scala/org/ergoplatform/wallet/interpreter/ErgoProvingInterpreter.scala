package org.ergoplatform.wallet.interpreter

import java.util

import org.ergoplatform._
import org.ergoplatform.validation.ValidationRules
import org.ergoplatform.wallet.protocol.context.{ErgoLikeParameters, ErgoLikeStateContext, TransactionContext}
import org.ergoplatform.wallet.secrets.ExtendedSecretKey
import sigmastate.Values.SigmaBoolean
import sigmastate.basics.DLogProtocol.{DLogInteractiveProver, DLogProverInput, ProveDlog}
import sigmastate.basics._
import sigmastate.eval.{IRContext, RuntimeIRContext}
import sigmastate.interpreter.{ContextExtension, HintsBag, ProverInterpreter}

import scala.util.{Failure, Success, Try}

/**
  * A class which is holding secrets and signing transactions.
  *
  * Currently it just generates some number of secrets (the number is provided via "dlogSecretsNumber" setting in the
  * "wallet" section) from a seed and sign a transaction (against input boxes to spend and
  * blockchain state) by using the secrets (no additional inputs, e.g. hash function preimages required in scripts,
  * are supported. Here, signing a transaction means spending proofs generation for all of its input boxes.
  *
  * @param secretKeys - secrets in extended form to be used by prover
  * @param params     - ergo network parameters
  */
class ErgoProvingInterpreter(val secretKeys: IndexedSeq[ExtendedSecretKey],
                             params: ErgoLikeParameters,
                             hintsBag: HintsBag)
                            (implicit IR: IRContext)
  extends ErgoInterpreter(params) with ProverInterpreter {

  val secrets: IndexedSeq[SigmaProtocolPrivateInput[_, _]] = secretKeys.map(_.key)

  val pubKeys: IndexedSeq[SigmaBoolean] = secrets.map(_.publicImage.asInstanceOf[SigmaBoolean])

  /**
    * A method which is generating a commitment to randomness, which is a first step to prove
    * knowledge of a secret. Method checks whether secret is known to the prover, and returns
    * None if the secret is not known.
    *
    * @param pubkey - public image of a secret
    * @return Some(cmt), a commitment to (secret) randomness, if the secret corresponding to pubkey is known,
    *         None otherwise
    */
  def generateCommitmentFor(pubkey: SigmaBoolean): Option[FirstProverMessage] = {
    val idx = pubKeys.indexOf(pubkey)
    if (idx == -1) {
      None
    } else {
      val s = secrets(idx)

      val proverOpt: Option[InteractiveProver[_, _, _]] = pubkey match {
        case dl: ProveDlog =>
          Some(new DLogInteractiveProver(dl, Some(s.asInstanceOf[DLogProverInput])))
        case dh: ProveDHTuple =>
          Some(new DiffieHellmanTupleInteractiveProver(dh, Some(s.asInstanceOf[DiffieHellmanTupleProverInput])))
        case _ => None
      }

      proverOpt.map(_.firstMessage)
    }
  }

  /** Requires `unsignedTx` inputs and `boxesToSpend` have the same boxIds in the same order */
  def sign(unsignedTx: UnsignedErgoLikeTransaction,
           boxesToSpend: IndexedSeq[ErgoBox],
           dataBoxes: IndexedSeq[ErgoBox],
           stateContext: ErgoLikeStateContext): Try[ErgoLikeTransaction] = {
    if (unsignedTx.inputs.length != boxesToSpend.length) Failure(new Exception("Not enough boxes to spend"))
    else if (unsignedTx.dataInputs.length != dataBoxes.length) Failure(new Exception("Not enough data boxes"))
    else boxesToSpend
      .zipWithIndex
      .foldLeft(Try(IndexedSeq[Input]() -> 0L)) { case (inputsCostTry, (inputBox, boxIdx)) =>
        val unsignedInput = unsignedTx.inputs(boxIdx)
        require(util.Arrays.equals(unsignedInput.boxId, inputBox.id))

        val transactionContext = TransactionContext(boxesToSpend, dataBoxes, unsignedTx, boxIdx.toShort)

        inputsCostTry.flatMap { case (ins, totalCost) =>

          val context = new ErgoLikeContext(
            stateContext.currentHeight,
            ErgoInterpreter.avlTreeFromDigest(stateContext.previousStateDigest),
            stateContext.lastBlockMinerPk,
            stateContext.sigmaLastHeaders,
            stateContext.sigmaPreHeader,
            transactionContext.dataBoxes,
            transactionContext.boxesToSpend,
            transactionContext.spendingTransaction,
            transactionContext.self,
            ContextExtension.empty,
            ValidationRules.currentSettings,
            params.maxBlockCost
          )

          prove(inputBox.ergoTree, context, unsignedTx.messageToSign, hintsBag).flatMap { proverResult =>
            val newTC = totalCost + proverResult.cost
            if (newTC > context.costLimit)
              Failure(new Exception(s"Cost of transaction $unsignedTx exceeds limit ${context.costLimit}"))
            else Success((Input(unsignedInput.boxId, proverResult) +: ins) -> newTC)
          }
        }
      }
      .map { case (inputs, _) =>
        new ErgoLikeTransaction(inputs.reverse, unsignedTx.dataInputs, unsignedTx.outputCandidates)
      }
  }

}

object ErgoProvingInterpreter {

  def apply(secrets: IndexedSeq[ExtendedSecretKey],
            params: ErgoLikeParameters,
            hints: HintsBag): ErgoProvingInterpreter =
    new ErgoProvingInterpreter(secrets, params, hints)(new RuntimeIRContext)

  def apply(rootSecret: ExtendedSecretKey,
            params: ErgoLikeParameters,
            hints: HintsBag): ErgoProvingInterpreter =
    new ErgoProvingInterpreter(IndexedSeq(rootSecret), params, hints)(new RuntimeIRContext)
}
