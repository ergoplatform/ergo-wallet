package org.ergoplatform.wallet.boxes

import org.ergoplatform.ErgoBox
import org.ergoplatform.wallet.boxes.BoxSelector.{BoxSelectionResult, subtractAssetsMut}
import scorex.util.ModifierId

import scala.annotation.tailrec
import scala.collection.mutable

class ReplaceCompactCollectBoxSelector(maxInputs: Int, optimalInputs: Int) extends BoxSelector {
  /**
    * A method which is selecting boxes to spend in order to collect needed amounts of ergo tokens and assets.
    *
    * @param inputBoxes    - unspent boxes to choose from.
    * @param filterFn      - user-provided filter function for boxes. From inputBoxes, only ones to be chosen for which
    *                      filterFn(box) returns true
    * @param targetBalance - ergo balance to be met
    * @param targetAssets  - assets balances to be met
    * @return None if select() is failing to pick appropriate boxes, otherwise Some(res), where res contains boxes
    *         to spend as well as monetary values and assets for boxes containing change
    *         (wrapped in a special BoxSelectionResult class).
    */
  override def select(inputBoxes: Iterator[TrackedBox],
                      filterFn: TrackedBox => Boolean,
                      targetBalance: Long,
                      targetAssets: Map[ModifierId, Long]): Option[BoxSelector.BoxSelectionResult] = {
    DefaultBoxSelector.select(inputBoxes, filterFn, targetBalance, targetAssets).flatMap { initialSelection =>
      val tail = inputBoxes.take(maxInputs * 3).toSeq

      val afterCompactionOpt = (if (initialSelection.boxes.length > maxInputs) {
        replace(initialSelection, tail, targetBalance, targetAssets)
      } else Some(initialSelection)).flatMap { afterReplacement =>
        if (afterReplacement.boxes.length > maxInputs) {
          compress(afterReplacement, targetBalance, targetAssets)
        } else Some(afterReplacement)
      }

      afterCompactionOpt.flatMap{ afterCompaction =>
        if (afterCompaction.boxes.length > maxInputs) {
          None
        } else if(afterCompaction.boxes.length < optimalInputs) {
          val diff = optimalInputs - afterCompaction.boxes.length
          val afterCompactionIds = afterCompaction.boxes.map(_.id).map(scorex.util.bytesToId)
          val dust = tail.sortBy(_.value).take(diff).filter(b => !afterCompactionIds.contains(b.boxId))

          val boxes = afterCompaction.boxes ++ dust.map(_.box)
          //todo: refactor below
          val compactedBoxes = boxes
          val compactedBalance = compactedBoxes.map(_.value).sum
          val compactedAssets = mutable.Map[ModifierId, Long]()
          BoxSelector.mergeAssetsMut(compactedAssets, compactedBoxes.map(BoxSelector.assetMap): _*)

          subtractAssetsMut(compactedAssets, targetAssets)
          val changeBoxesAssets: Seq[mutable.Map[ModifierId, Long]] = compactedAssets.grouped(ErgoBox.MaxTokens).toSeq
          val changeBalance = compactedBalance - targetBalance
          formChangeBoxes(changeBalance, changeBoxesAssets).map(changeBoxes => BoxSelectionResult(compactedBoxes, changeBoxes))

        } else Some(afterCompaction)
      }
    }
  }

  def compress(bsr: BoxSelectionResult,
               targetBalance: Long,
               targetAssets: Map[ModifierId, Long]): Option[BoxSelectionResult] = {
    val boxes = bsr.boxes
    val diff = boxes.map(_.value).sum - targetBalance

    val boxesToThrowAway = boxes.filter(!_.additionalTokens.toArray.map(_._1).exists(tid => targetAssets.keySet.contains(scorex.util.bytesToId(tid))))
    val sorted = boxesToThrowAway.sortBy(_.value)

    if (diff >= sorted.head.value) {
      var thrownValue = 0L
      val thrownBoxes = sorted.takeWhile { b =>
        thrownValue = thrownValue + b.value
        thrownValue <= diff
      }

      //todo: refactor below
      val compactedBoxes = boxes.filter(b => !thrownBoxes.contains(b))
      val compactedBalance = compactedBoxes.map(_.value).sum
      val compactedAssets = mutable.Map[ModifierId, Long]()
      BoxSelector.mergeAssetsMut(compactedAssets, compactedBoxes.map(BoxSelector.assetMap): _*)

      subtractAssetsMut(compactedAssets, targetAssets)
      val changeBoxesAssets: Seq[mutable.Map[ModifierId, Long]] = compactedAssets.grouped(ErgoBox.MaxTokens).toSeq
      val changeBalance = compactedBalance - targetBalance
      formChangeBoxes(changeBalance, changeBoxesAssets).map(changeBoxes => BoxSelectionResult(compactedBoxes, changeBoxes))
    } else Some(bsr)
  }

  def replace(bsr: BoxSelectionResult,
              tail: Seq[TrackedBox],
              targetBalance: Long,
              targetAssets: Map[ModifierId, Long]): Option[BoxSelectionResult] = {
    val bigBoxes = tail.sortBy(-_.value).map(_.box)
    val boxesToThrowAway = bsr.boxes.filter(!_.additionalTokens.toArray.map(_._1).exists(tid => targetAssets.keySet.contains(scorex.util.bytesToId(tid))))
    val sorted = boxesToThrowAway.sortBy(_.value)

    type BoxesToAdd = Seq[ErgoBox]
    type BoxesToDrop = Seq[ErgoBox]
    type Operations = (BoxesToAdd, BoxesToDrop)

    @tailrec
    def replaceStep(candidates: Seq[ErgoBox], toDrop: Seq[ErgoBox], currentOps: Operations): Operations = {
      candidates match {
        case Seq() => currentOps
        case Seq(cand) if cand.value <= toDrop.headOption.map(_.value).getOrElse(Long.MaxValue) => currentOps
        case Seq(cand, cs@_*) =>
          var collected = 0L
          val (dropped, remain) = toDrop.partition { b =>
            collected = collected + b.value
            collected <= cand.value
          }
          replaceStep(cs, remain, (currentOps._1 :+ cand, currentOps._2 ++ dropped))
      }
    }

    val (toAdd, toDrop) = replaceStep(bigBoxes, sorted, (Seq(), Seq()))
    if (toAdd.nonEmpty) {
      //todo: refactor below
      val compactedBoxes = bsr.boxes.filter(b => !toDrop.contains(b)) ++ toAdd
      val compactedBalance = compactedBoxes.map(_.value).sum
      val compactedAssets = mutable.Map[ModifierId, Long]()
      BoxSelector.mergeAssetsMut(compactedAssets, compactedBoxes.map(BoxSelector.assetMap): _*)

      subtractAssetsMut(compactedAssets, targetAssets)
      val changeBoxesAssets: Seq[mutable.Map[ModifierId, Long]] = compactedAssets.grouped(ErgoBox.MaxTokens).toSeq
      val changeBalance = compactedBalance - targetBalance
      formChangeBoxes(changeBalance, changeBoxesAssets).map(changeBoxes => BoxSelectionResult(compactedBoxes, changeBoxes))
    } else Some(bsr)
  }
}
