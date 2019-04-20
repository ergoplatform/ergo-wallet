package org.ergoplatform.wallet.boxes

sealed abstract class BoxCertainty(val certain: Boolean)

object BoxCertainty {

  case object Certain extends BoxCertainty(certain = true)

  case object Uncertain extends BoxCertainty(certain = false)

}

sealed abstract class ChainStatus(val mainChain: Boolean)

object ChainStatus {

  case object MainChain extends ChainStatus(mainChain = true)

  case object Fork extends ChainStatus(mainChain = false)

}

sealed abstract class SpendingStatus(val spent: Boolean)

object SpendingStatus {

  case object Spent extends SpendingStatus(spent = true)

  case object Unspent extends SpendingStatus(spent = false)

}