package org.ergoplatform.wallet.boxes

import org.ergoplatform.ErgoBox
import scorex.util.ModifierId

import scala.collection.mutable

/**
  * An interface which is exposing a method to select unspent boxes according to target amounts in Ergo tokens and
  * assets and possible user-defined filter. The interface could have many instantiations implementing
  * different strategies.
  */
trait BoxSelector {

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
  def select(inputBoxes: Iterator[TrackedBox],
             filterFn: TrackedBox => Boolean,
             targetBalance: Long,
             targetAssets: Map[ModifierId, Long]): Option[BoxSelector.BoxSelectionResult]
}

object BoxSelector {

  final case class BoxSelectionResult(boxes: Seq[ErgoBox],
                                      changeBoxes: Seq[(Long, Map[ModifierId, Long])])

  @inline
  def mergeAssetsMut(into: mutable.Map[ModifierId, Long], from: Map[ModifierId, Long]): Unit = {
    from.foreach { case (id, amount) =>
      into.put(id, into.getOrElse(id, 0L) + amount)
    }
  }

  @inline
  def mergeAssets(from: Map[ModifierId, Long], to: Map[ModifierId, Long] = Map.empty): Map[ModifierId, Long] = {
    from.foldLeft(to) { case (acc, (id, amount)) =>
      acc.updated(id, acc.getOrElse(id, 0L) + amount)
    }
  }

  @inline
  def subtractAssetsMut(from: mutable.Map[ModifierId, Long], subtractor: Map[ModifierId, Long]): Unit = {
    subtractor.foreach { case (id, subtractAmt) =>
      val fromAmt = from(id)
      if (fromAmt == subtractAmt) {
        from.remove(id)
      } else {
        from.put(id, fromAmt - subtractAmt)
      }
    }
  }

}
