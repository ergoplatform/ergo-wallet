package org.ergoplatform.wallet.secrets

import scala.util.{Failure, Try}

/**
  * HD key derivation path (see: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki)
  */
final case class DerivationPath(decodedPath: List[Int], publicBranch: Boolean) {

  import DerivationPath._

  def depth: Int = decodedPath.length

  def index: Long = decodedPath.last

  def isMaster: Boolean = depth == 1

  def encoded: String =
    (if (publicBranch) s"$PublicBranchMasterId/" else s"$PrivateBranchMasterId/") + decodedPath.tail
      .map(x => if (Index.isHardened(x)) s"${x - Index.HardRangeStart}'" else x.toString)
      .mkString("/")

  def extended(idx: Int): DerivationPath = DerivationPath(decodedPath :+ idx, publicBranch)

  def toPublic: DerivationPath = this.copy(publicBranch = true)

  override def toString: String = encoded
}

object DerivationPath {

  val PublicBranchMasterId = "M"
  val PrivateBranchMasterId = "m"

  val MasterPath: DerivationPath = DerivationPath(List(0), publicBranch = false)

  def fromEncoded(path: String): Try[DerivationPath] = {
    val split = path.split("/")
    if (!split.headOption.exists(Seq(PublicBranchMasterId, PrivateBranchMasterId).contains)) {
      Failure(new Exception("Wrong path format"))
    } else {
      val pathTry = split.tail.foldLeft(Try(List(0))) { case (accTry, sym) =>
        accTry.flatMap { acc =>
          Try(if (sym.endsWith("'")) Index.hardIndex(sym.dropRight(1).toInt) else sym.toInt)
            .map(acc :+ _)
        }
      }
      val isPublicBranch = split.head == PublicBranchMasterId
      pathTry.map(DerivationPath(_, isPublicBranch))
    }
  }

}
