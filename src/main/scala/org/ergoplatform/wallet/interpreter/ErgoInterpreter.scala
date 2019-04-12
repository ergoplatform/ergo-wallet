package org.ergoplatform.wallet.interpreter

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.wallet.protocol.Constants
import org.ergoplatform.wallet.protocol.context.{ErgoContext, ErgoLikeParameters}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoLikeInterpreter}
import sigmastate.Values.ErgoTree
import sigmastate.eval.{IRContext, RuntimeIRContext}
import sigmastate.interpreter.Interpreter.{ScriptEnv, VerificationResult}

import scala.util.Try

/**
  * ErgoTree language interpreter, Ergo version. In addition to ErgoLikeInterpreter, it contains
  * rules for expired boxes spending validation.
  *
  * @param params - current values of adjustable blockchain settings
  */
class ErgoInterpreter(params: ErgoLikeParameters)(implicit IR: IRContext)
  extends ErgoLikeInterpreter(params.maxBlockCost) {

  override type CTX = ErgoContext

  /**
    * Check that expired box is spent in a proper way
    *
    * @param box    - box being spent
    * @param output - newly created box
    * @param currentHeight - current height of the blockchain (at the moment of spending)
    * @return whether the box is spent properly according to the storage fee rule
    */
  protected def checkExpiredBox(box: ErgoBox, output: ErgoBoxCandidate, currentHeight: Height): Boolean = {
    val maxStorageFee = params.storageFeeFactor * box.bytes.length

    val storageFeeCovered = box.value - maxStorageFee <= 0
    val correctCreationHeight = output.creationHeight == currentHeight
    val correctOutValue = output.value >= box.value - maxStorageFee
    val correctRegisters = ErgoBox.allRegisters.tail
      .forall(rId => rId == ErgoBox.ReferenceRegId || box.get(rId) == output.get(rId))

    storageFeeCovered || (correctCreationHeight && correctOutValue && correctRegisters)
  }

  override def verify(env: ScriptEnv,
                      exp: ErgoTree,
                      context: CTX,
                      proof: Array[Byte],
                      message: Array[Byte]): Try[VerificationResult] = {

    lazy val varId = Constants.StorageIndexVarId

    //no proof provided and enough time since box creation to spend it
    if (context.currentHeight - context.self.creationHeight >= Constants.StoragePeriod
      && proof.length == 0
      && context.extension.values.contains(varId)) {

      Try {
        val idx = context.extension.values(varId).value.asInstanceOf[Short]
        val outputCandidate = context.spendingTransaction.outputCandidates(idx)

        checkExpiredBox(context.self, outputCandidate, context.currentHeight) -> Constants.StorageContractCost
      }.recoverWith { case _ => super.verify(env, exp, context, proof, message) }
    } else {
      super.verify(env, exp, context, proof, message)
    }
  }
}

object ErgoInterpreter {

  def apply(params: ErgoLikeParameters): ErgoInterpreter =
    new ErgoInterpreter(params)(new RuntimeIRContext)
}
