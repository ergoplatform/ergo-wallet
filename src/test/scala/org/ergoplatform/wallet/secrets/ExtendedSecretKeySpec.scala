package org.ergoplatform.wallet.secrets

import org.scalatest.{Matchers, PropSpec}
import scorex.util.encode.Base58

class ExtendedSecretKeySpec extends PropSpec with Matchers {

  property("master key derivation from seed") {
    val expectedRoot = "4rEDKLd17LX4xNR8ss4ithdqFRc3iFnTiTtQbanWJbCT"
    val expectedKey11 = "CLdMMHxNtiPzDnWrVuZQr22VyUx8deUG7vMqMNW7as7M"
    val expectedKey22 = "9icjp3TuTpRaTn6JK6AHw2nVJQaUnwmkXVdBdQSS98xD"
    val expectedKey3Hard2 = "DWMp3L9JZiywxSb5gSjc5dYxPwEZ6KkmasNiHD6VRcpJ"
    val seedStr = "5KgzUvF4yZjBDkoseNyZnAHsA6cuqhvwkGUNZ4y2WQXfRhLoHfbipZR4XriVZKbVFMcP6QKLRwLZhJdRt2wKx6tY"
    val seed = Base58.decode(seedStr).get

    val root = ExtendedSecretKey.fromSeed(seed)
    val k11 = root.child(1)
    val k22 = k11.child(2)
    val k3h2 = k22.child(Index.hardIndex(2))

    Base58.encode(root.keyBytes) shouldEqual expectedRoot
    Base58.encode(k11.keyBytes) shouldEqual expectedKey11
    Base58.encode(k22.keyBytes) shouldEqual expectedKey22
    Base58.encode(k3h2.keyBytes) shouldEqual expectedKey3Hard2
  }

}
