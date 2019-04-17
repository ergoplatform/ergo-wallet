package org.ergoplatform.wallet.secrets

import org.scalatest.{Matchers, PropSpec}
import scorex.util.encode.Base58

class ExtendedSecretKeySpec extends PropSpec with Matchers {

  property("master key derivation from seed") {
    val expectedKey = "4rEDKLd17LX4xNR8ss4ithdqFRc3iFnTiTtQbanWJbCT"
    val seedStr = "5KgzUvF4yZjBDkoseNyZnAHsA6cuqhvwkGUNZ4y2WQXfRhLoHfbipZR4XriVZKbVFMcP6QKLRwLZhJdRt2wKx6tY"
    val seed = Base58.decode(seedStr).get

    Base58.encode(ExtendedSecretKey.fromSeed(seed).keyBytes) shouldEqual expectedKey
  }

}
