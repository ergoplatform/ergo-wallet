package org.ergoplatform.wallet.protocol.context

import scorex.crypto.authds.ADDigest
import special.collection.Coll

case class ErgoLikeStateContext(currentHeight: Int,
                                previousStateDigest: ADDigest,
                                lastBlockMinerPk: Array[Byte],
                                sigmaLastHeaders: Coll[special.sigma.Header],
                                sigmaPreHeader: special.sigma.PreHeader)
