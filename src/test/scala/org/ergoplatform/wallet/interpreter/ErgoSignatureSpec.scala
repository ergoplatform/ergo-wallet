package org.ergoplatform.wallet.interpreter

import org.ergoplatform.wallet.utils.Generators
import org.scalatest.{Matchers, PropSpec}
import scorex.util.Random
import sigmastate.basics.DLogProtocol.DLogProverInput

class ErgoSignatureSpec extends PropSpec with Matchers with Generators {

  import ErgoSignature._

  property("sign/verify") {

    val secret = DLogProverInput(genSecret.bigInteger)
    val pk = secret.publicImage

    val msg = Random.randomBytes(128)

    val sig = sign(msg, secret.w)

    verify(msg, sig, pk.h) shouldBe true
  }

}
