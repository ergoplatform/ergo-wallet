package org.ergoplatform.wallet.boxes

import org.ergoplatform.ErgoBox
import org.ergoplatform.wallet.serialization.ErgoWalletSerializer
import scorex.util.serialization.{Reader, Writer}
import sigmastate.SBox
import sigmastate.serialization.{ConstantStore, DataSerializer}
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

object ErgoBoxSerializer extends ErgoWalletSerializer[ErgoBox] {

  override def serialize(box: ErgoBox, w: Writer): Unit = {
    val writer = new SigmaByteWriter(w, None)
    DataSerializer.serialize[SBox.type](box, SBox, writer)
  }

  override def parse(r: Reader): ErgoBox = {
    val reader = new SigmaByteReader(r, new ConstantStore(), resolvePlaceholdersToConstants = false)
    DataSerializer.deserialize(SBox, reader)
  }

}
