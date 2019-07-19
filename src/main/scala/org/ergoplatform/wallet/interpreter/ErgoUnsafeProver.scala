package org.ergoplatform.wallet.interpreter

import org.ergoplatform.{ErgoLikeTransaction, Input, UnsignedErgoLikeTransaction}
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.interpreter.{ContextExtension, ProverResult}

/**
  * A naive Ergo prover implementation not performing transaction cost verification.
  *
  * @note this prover is only suitable for signing only small number of simple inputs,
  *       for inputs with complex scripts use `ErgoProvingInterpreter`
  */
class ErgoUnsafeProver {

  import org.ergoplatform.wallet.crypto.ErgoSignature._

  def prove(unsignedTx: UnsignedErgoLikeTransaction,
            sk: DLogProverInput): ErgoLikeTransaction = {
    val sig = ProverResult(sign(unsignedTx.messageToSign, sk.w), ContextExtension.empty)
    val inputs = unsignedTx.inputs.map { unsignedInput =>
      Input(unsignedInput.boxId, sig)
    }
    new ErgoLikeTransaction(inputs, unsignedTx.dataInputs, unsignedTx.outputCandidates)
  }

}