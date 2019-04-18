package org.ergoplatform.wallet.boxes

import org.ergoplatform.{ErgoBox, ErgoLikeTransaction}
import scorex.util.{ModifierId, bytesToId}

/**
  * A box tracked by a wallet that contains Ergo box itself as well as
  * its state (e.g. spent or not, confirmed or not etc).
  *
  * @param creationTxId     - Id of transaction created the box
  * @param creationOutIndex - Output index in the creation transaction
  * @param inclusionHeight  - Height the transaction was included into blockchain
  * @param spendingTxIdOpt  - Id of transaction which spends the box if exists and known
  * @param spendingHeight   - Height of the spending transaction block in blockchain if known
  * @param box              - Underlying Ergo box
  * @param certainty        - Whether the box is definitely belongs to the user or not
  */
final case class TrackedBox(creationTxId: ModifierId,
                            creationOutIndex: Short,
                            inclusionHeight: Option[Int],
                            spendingTxIdOpt: Option[ModifierId],
                            spendingHeight: Option[Int],
                            box: ErgoBox,
                            certainty: BoxCertainty) {

  /**
    * Whether the box is spent or not
    */
  def spendingStatus: SpendingStatus =
    if (spendingTxIdOpt.isEmpty) SpendingStatus.Unspent
    else SpendingStatus.Spent

  /**
    * Whether box creation is confirmed or not.
    * Can be derived from `spendingStatus` and `chainStatus` combination
    */
  def creationChainStatus: ChainStatus =
    if (inclusionHeight.isEmpty) ChainStatus.Fork
    else ChainStatus.MainChain

  /**
    * Whether box spending is confirmed or not, `Offchain` for unspent boxes.
    * Can be derived from `spendingStatus` and `chainStatus` combination
    */
  def spendingChainStatus: ChainStatus =
    if (spendingStatus == SpendingStatus.Unspent || spendingHeight.isEmpty) ChainStatus.Fork
    else ChainStatus.MainChain

  /**
    * Same as `creationChainStatus` for unspent boxes,
    * same as `spendingChainStatus` for spent boxes
    */
  def chainStatus: ChainStatus =
    if (creationChainStatus == ChainStatus.Fork || spendingStatus == SpendingStatus.Spent &&
      spendingChainStatus == ChainStatus.Fork) ChainStatus.Fork
    else ChainStatus.MainChain

  def boxId: ModifierId = bytesToId(box.id)

  def value: Long = box.value

  def assets: Map[ModifierId, Long] = box.additionalTokens.map { case (id, amt) =>
    bytesToId(id) -> amt
  }.toMap

}

object TrackedBox {

  def apply(creationTx: ErgoLikeTransaction, creationOutIndex: Short, creationHeight: Option[Int],
            box: ErgoBox, certainty: BoxCertainty): TrackedBox =
    apply(creationTx.id, creationOutIndex, creationHeight, None, None, box, certainty)
}
