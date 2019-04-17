package org.ergoplatform.wallet.secrets

import scala.util.{Failure, Try}

/**
  * HD key derivation path (see: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki)
  */
final case class DerivationPath(decodedPath: List[Long], publicBranch: Boolean) {

  import DerivationPath._

  def depth: Int = decodedPath.length

  def index: Long = decodedPath.last

  def isMaster: Boolean = depth == 1

  def encoded: String =
    (if (publicBranch) s"$PublicBranchMasterId/" else s"$PrivateBranchMasterId/") + decodedPath.tail
      .map(x => if (x >= HardenedIndexRangeStart) s"${x - HardenedIndexRangeStart}'" else x.toString)
      .mkString("/")

  def extended(idx: Long): DerivationPath = DerivationPath(decodedPath :+ idx, publicBranch)

  override def toString: String = encoded
}

object DerivationPath {

  val PublicBranchMasterId = "M"
  val PrivateBranchMasterId = "m"

  val HardenedIndexRangeStart = 2147483648L // 2^31
  val HardenedIndexRangeEnd = 4294967295L // 2^32 - 1

  def masterPath: DerivationPath = DerivationPath(List(0), publicBranch = false)

  def fromEncoded(path: String): Try[DerivationPath] = {
    val split = path.split("/")
    if (!split.headOption.exists(Seq(PublicBranchMasterId, PrivateBranchMasterId).contains)) {
      Failure(new Exception("Wrong path format"))
    } else {
      val pathTry = split.tail.foldLeft(Try(List(0L))) { case (accTry, sym) =>
        accTry.flatMap { acc =>
          Try(if (sym.endsWith("'")) HardenedIndexRangeStart + sym.dropRight(1).toLong else sym.toLong)
            .map(acc :+ _)
        }
      }
      val isPublicBranch = split.head == PublicBranchMasterId
      if (path.forall(x => x >= 0 && x <= HardenedIndexRangeEnd)) pathTry.map(DerivationPath(_, isPublicBranch))
      else Failure(new Exception("Unbound index in path"))
    }
  }

}
