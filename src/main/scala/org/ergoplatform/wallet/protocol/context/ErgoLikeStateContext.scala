package org.ergoplatform.wallet.protocol.context

import scorex.crypto.authds.ADDigest
import special.collection.Coll

trait ErgoLikeStateContext {

  def currentHeight: Int

  def previousStateDigest: ADDigest

  def lastBlockMinerPk: Array[Byte]

  def sigmaLastHeaders: Coll[special.sigma.Header]

  def sigmaPreHeader: special.sigma.PreHeader

}
