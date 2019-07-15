package org.ergoplatform.wallet.demo

import org.ergoplatform._
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.ergoplatform.wallet.protocol.context.{ErgoLikeParameters, ErgoLikeStateContext}
import org.ergoplatform.wallet.secrets.ExtendedSecretKey
import org.scalatest.PropSpec
import scorex.crypto.authds.ADDigest
import scorex.util.ModifierId
import scorex.util.encode.Base16
import scorex.utils.Random
import sigmastate.Values.TrueLeaf
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.serialization.ErgoTreeSerializer
import special.collection.Coll
import special.sigma.{Header, PreHeader}

import scala.util.{Failure, Success}

class TransactionSigningDemo extends PropSpec {

  property("Transaction signing (local db)") {
    val encoder = ErgoAddressEncoder(0x00: Byte)

    val receiverAddressStr = "9fKYyGuV3wMYFYzWBR1FDgc61CFV2hbGLrY6S3wgH1r4xJcwLnq"
    val receiverAddress = encoder.fromString(receiverAddressStr) match {
      case Success(addr) => addr
      case _ => throw new Exception("Invalid address")
    }

    // Create address
    val entropy = Random.randomBytes(32)
    val extendedSecretKey = ExtendedSecretKey.deriveMasterKey(entropy)
    val address = P2PKAddress(extendedSecretKey.key.publicImage)(encoder)

    // Fetch from `/info` API route of public node once in epoch (1024 blocks period).
    // Example:
    // GET `<public_node_ip>:<port>/info`
    // Response:
    // {
    //  "currentTime" : 1563186022388,
    //  "name" : "ergo-mainnet-andyceo",
    //  "stateType" : "utxo",
    //  "difficulty" : 173864571109376,
    //  "bestFullHeaderId" : "635347ef8b1a7d93ff99f5724b70c1edcfe351271f2566d943785efd51044b9b",
    //  "bestHeaderId" : "635347ef8b1a7d93ff99f5724b70c1edcfe351271f2566d943785efd51044b9b",
    //  "peersCount" : 24,
    //  "unconfirmedCount" : 0,
    //  "appVersion" : "3.0.0",
    //  "stateRoot" : "d2f33ce42444706642bda482c36b48ba397efe7de9d4a98d8f78a86edfb583d310",
    //  "genesisBlockId" : "b0244dfc267baca974a4caee06120321562784303a8a688976ae56170e4d175b",
    //  "previousFullHeaderId" : "ef3d443b2a724c6621dcd06136692cf5d51ea516c3efe28b778d6aedf453082c",
    //  "fullHeight" : 10185,
    //  "headersHeight" : 10185,
    //  "stateVersion" : "635347ef8b1a7d93ff99f5724b70c1edcfe351271f2566d943785efd51044b9b",
    //  "fullBlocksScore" : 1385268536556912640,
    //  "launchTime" : 1561988443477,
    //  "headersScore" : 1385268536556912640,
    //  "parameters" : {
    //    "outputCost" : 100,
    //    "tokenAccessCost" : 100,
    //    "maxBlockCost" : 1000000,
    //    "height" : 9216,
    //    "maxBlockSize" : 524288,
    //    "dataInputCost" : 100,
    //    "blockVersion" : 1,
    //    "inputCost" : 2000,
    //    "storageFeeFactor" : 1250000,
    //    "minValuePerByte" : 360
    //  },
    //  "isMining" : true
    //}
    val parameters = new ErgoLikeParameters {

      // All properties are taken from `jsonResponse["parameters"]`

      override def storageFeeFactor: Int = 1250000

      override def minValuePerByte: Int = 360

      override def maxBlockSize: Int = 524288

      override def tokenAccessCost: Int = 100

      override def inputCost: Int = 2000

      override def dataInputCost: Int = 100

      override def outputCost: Int = 100

      override def maxBlockCost: Long = 1000000

      override def softForkStartingHeight: Option[Int] = None

      override def softForkVotesCollected: Option[Int] = None

      override def blockVersion: Byte = 1
    }

    // Create prover
    val prover = ErgoProvingInterpreter(extendedSecretKey, parameters)

    val value = 2000000 // How many ERGs you want to transfer
    val currentHeight = 8934 // Current height. Fetch form public node `/info API` (`fullHeight` field in response)

    val payTo = new ErgoBoxCandidate(
      value,
      receiverAddress.script,
      currentHeight,
      Seq.empty[(ErgoBox.TokenId, Long)].toColl,
      Map.empty
    )

    // Create fee payment
    val feeAmount = 1000000 // minimal fee amount
    val fee = new ErgoBoxCandidate(
      feeAmount,
      TrueLeaf.toSigmaProp,
      currentHeight,
      Seq.empty[(ErgoBox.TokenId, Long)].toColl,
      Map.empty
    )

    val targetPayoutValue = fee.value + payTo.value

    // Assuming you have the following structure for each input in your db
    case class RawInputInfo(
      value: Long,
      ergoTreeRaw: String, // `ergoTree` field from json box repr
      transactionId: String, // id of transaction containing this input
      index: Short, // index in the outputs list in transaction
      creationHeight: Int // height of the block containing transaction with this input
    )

    val availableInputs: Seq[RawInputInfo] = ??? // fetchFromDb.resultAs[Seq[RawInputInfo]]

    var collectedValue = 0L
    val inputsToUse: IndexedSeq[ErgoBox] = availableInputs
      .takeWhile { inputInfo =>
        collectedValue += inputInfo.value
        collectedValue >= targetPayoutValue
      }
      .map { inputInfo =>
        val bytes = Base16.decode(inputInfo.ergoTreeRaw).getOrElse(throw new Exception("Failed to decode input id"))
        val ergoTree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)
        ErgoBox(
          inputInfo.value,
          ergoTree,
          transactionId = ModifierId.@@(inputInfo.transactionId),
          creationHeight = inputInfo.creationHeight,
          boxIndex = inputInfo.index
        )
      }
      .toIndexedSeq

    // Create change payment
    val changeAmount = collectedValue - targetPayoutValue
    val change = new ErgoBoxCandidate(
      changeAmount,
      address.script,
      currentHeight,
      Seq.empty[(ErgoBox.TokenId, Long)].toColl,
      Map.empty
    )

    val unsignedInputs = inputsToUse
      .map { box =>
        new UnsignedInput(box.id)
      }
      .toIndexedSeq
    val unsignedTx = new UnsignedErgoLikeTransaction(
      unsignedInputs,
      IndexedSeq(),
      IndexedSeq(payTo, change, fee)
    )

    val stateContext: ErgoLikeStateContext = new ErgoLikeStateContext {

      override def currentHeight: Int = 0

      override def sigmaLastHeaders: Coll[Header] = Colls.emptyColl

      override def previousStateDigest: ADDigest = Base16.decode("a5df145d41ab15a01e0cd3ffbab046f0d029e5412293072ad0f5827428589b9302")
        .fold(_ => throw new Error(s"Failed to parse genesisStateDigest"), ADDigest @@ _)

      override def sigmaPreHeader: PreHeader = null

      override def lastBlockMinerPk: Array[Byte] = null
    }


    val tx = prover.sign(unsignedTx, inputsToUse, IndexedSeq(), stateContext) match {
      case Success(transaction) => transaction
      case Failure(exception) => throw exception
    }

    // Serialize tx to JSON and send it to the node
    // httpRequest("<public_node_ip>:<port>/transactions", POST, body = tx.asJson)
  }

  property("Transaction signing (explorer)") {
    val encoder = ErgoAddressEncoder(0x00: Byte)

    val receiverAddressStr = "9fKYyGuV3wMYFYzWBR1FDgc61CFV2hbGLrY6S3wgH1r4xJcwLnq"
    val receiverAddress = encoder.fromString(receiverAddressStr) match {
      case Success(addr) => addr
      case _ => throw new Exception("Invalid address")
    }

    // Create address
    val entropy = Random.randomBytes(32)
    val extendedSecretKey = ExtendedSecretKey.deriveMasterKey(entropy)
    val address = P2PKAddress(extendedSecretKey.key.publicImage)(encoder)

    // Fetch from `/info` API route of public node once in epoch (1024 blocks period).
    // Example:
    // GET `<public_node_ip>:<port>/info`
    // Response:
    // {
    //  "currentTime" : 1563186022388,
    //  "name" : "ergo-mainnet-andyceo",
    //  "stateType" : "utxo",
    //  "difficulty" : 173864571109376,
    //  "bestFullHeaderId" : "635347ef8b1a7d93ff99f5724b70c1edcfe351271f2566d943785efd51044b9b",
    //  "bestHeaderId" : "635347ef8b1a7d93ff99f5724b70c1edcfe351271f2566d943785efd51044b9b",
    //  "peersCount" : 24,
    //  "unconfirmedCount" : 0,
    //  "appVersion" : "3.0.0",
    //  "stateRoot" : "d2f33ce42444706642bda482c36b48ba397efe7de9d4a98d8f78a86edfb583d310",
    //  "genesisBlockId" : "b0244dfc267baca974a4caee06120321562784303a8a688976ae56170e4d175b",
    //  "previousFullHeaderId" : "ef3d443b2a724c6621dcd06136692cf5d51ea516c3efe28b778d6aedf453082c",
    //  "fullHeight" : 10185,
    //  "headersHeight" : 10185,
    //  "stateVersion" : "635347ef8b1a7d93ff99f5724b70c1edcfe351271f2566d943785efd51044b9b",
    //  "fullBlocksScore" : 1385268536556912640,
    //  "launchTime" : 1561988443477,
    //  "headersScore" : 1385268536556912640,
    //  "parameters" : {
    //    "outputCost" : 100,
    //    "tokenAccessCost" : 100,
    //    "maxBlockCost" : 1000000,
    //    "height" : 9216,
    //    "maxBlockSize" : 524288,
    //    "dataInputCost" : 100,
    //    "blockVersion" : 1,
    //    "inputCost" : 2000,
    //    "storageFeeFactor" : 1250000,
    //    "minValuePerByte" : 360
    //  },
    //  "isMining" : true
    //}
    val parameters = new ErgoLikeParameters {

      // All properties are taken from `jsonResponse["parameters"]`

      override def storageFeeFactor: Int = 1250000

      override def minValuePerByte: Int = 360

      override def maxBlockSize: Int = 524288

      override def tokenAccessCost: Int = 100

      override def inputCost: Int = 2000

      override def dataInputCost: Int = 100

      override def outputCost: Int = 100

      override def maxBlockCost: Long = 1000000

      override def softForkStartingHeight: Option[Int] = None

      override def softForkVotesCollected: Option[Int] = None

      override def blockVersion: Byte = 1
    }

    // Create prover
    val prover = ErgoProvingInterpreter(extendedSecretKey, parameters)

    val value = 2000000 // How many ERGs you want to transfer
    val currentHeight = 8934 // Current height. Fetch form public node `/info API` (`fullHeight` field in response)

    val payTo = new ErgoBoxCandidate(
      value,
      receiverAddress.script,
      currentHeight,
      Seq.empty[(ErgoBox.TokenId, Long)].toColl,
      Map.empty
    )

    // Create fee payment
    val feeAmount = 1000000 // minimal fee amount
    val fee = new ErgoBoxCandidate(
      feeAmount,
      TrueLeaf.toSigmaProp,
      currentHeight,
      Seq.empty[(ErgoBox.TokenId, Long)].toColl,
      Map.empty
    )

    val targetPayoutValue = fee.value + payTo.value

    val availableInputs: Seq[(String, Long, String)] = ??? // fetchHttp(
    // "https://api.ergoplatform.com/transactions/boxes/byAddress/unspent/${address.toString}")
    // .responseAs[Seq[Json]]
    // .foreach(jsonChunk => jsonChunk.extractFieldsFromJson("id", "value", "ergoTree"))
    // see spec here: https://github.com/ergoplatform/explorer-back/blob/26a012960e79f195b9ab6b20962373b398edf93b/src/main/resources/api/openapi.yaml#L222

    var collectedValue = 0L
    val inputsToUse: IndexedSeq[ErgoBox] = availableInputs
      .takeWhile { case (_, amt, _) =>
        collectedValue += amt
        collectedValue >= targetPayoutValue
      }
      .map { case (id, amt, ergoTreeStr) =>
        val bytes = Base16.decode(ergoTreeStr).getOrElse(throw new Exception("Failed to decode input id"))
        ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)
        ??? // can't instantiate ErgoBox(amt, )
      }
      .toIndexedSeq

    // Create change payment
    val changeAmount = collectedValue - targetPayoutValue
    val change = new ErgoBoxCandidate(
      changeAmount,
      address.script,
      currentHeight,
      Seq.empty[(ErgoBox.TokenId, Long)].toColl,
      Map.empty
    )

    val unsignedInputs = inputsToUse
      .map { box =>
        new UnsignedInput(box.id)
      }
      .toIndexedSeq
    val unsignedTx = new UnsignedErgoLikeTransaction(
      unsignedInputs,
      IndexedSeq(),
      IndexedSeq(payTo, change, fee)
    )

    val stateContext: ErgoLikeStateContext = ???

    prover.sign(unsignedTx, inputsToUse, IndexedSeq(), stateContext)

  }

}
