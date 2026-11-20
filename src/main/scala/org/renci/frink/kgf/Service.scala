package org.renci.frink.kgf

import org.renci.frink.s2.S2Graph
import sttp.model.Uri
import sttp.model.Uri.*

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*
import scala.util.Try
import scala.util.Using

/** The identity of this server and the release it is.
  *
  * A bundle-backed dataset versions its data; here the data is the program, so a release of the program is a release of the dataset. Every
  * answer is a function of the generator and the S2 library it links, so the version is the release and the content digest is taken over
  * the code the process runs — which is what a content digest has to mean for cursors and ETags to be stable across two deployments of one
  * release (KGF doc 05 section 5.8). See `docs/kgf.md` section 1.
  */
object Service:

  /** The OKN registry shortname (`okn-registry/docs/registry/kgs/s2.md`) */
  val DatasetId = "s2"

  val DatasetIRI = "https://purl.org/okn/frink/kg/s2"

  val Title = "S2 Grid"

  val Description =
    "The complete S2 Geometry cell hierarchy, levels 0 to 30, as RDF: every cell's polygon and area, the cells that contain it and that " +
      "it contains, its neighbours at the same level, and one named graph per level. Nothing is stored; every quad is computed on request " +
      "and every count is exact."

  val Homepage = "https://github.com/frink-okn/s2-qpf"

  val License = "https://creativecommons.org/publicdomain/zero/1.0/"

  /** Keep in step with `publish.version` in `project.scala`, which `ServiceSpec` checks */
  val Release = "2.0.0"

  val Version = s"v$Release"

  /** Declared capabilities beside the mandatory core profile (KGF doc 03 section 3.1 principle 5) */
  val Capabilities = Vector("graphs", "terms")

  /** The prefix map a request's CURIEs resolve against, and the manifest publishes. `kwg-s2` and `kwg-s2geom` are the federation's shared
    * prefixes for cells and their geometries (KGF doc 05 section 5.8), which is how a routing index tells this server's IRIs from other
    * KnowWhereGraph resources under `kwg:`.
    */
  val Prefixes: Seq[(String, String)] = Vector(
    "geo" -> S2Graph.GeoSPARQL,
    "kwg" -> S2Graph.KWGResource,
    "kwg-ont" -> S2Graph.KWGOnt,
    "kwg-s2" -> s"${S2Graph.KWGResource}s2.",
    "kwg-s2geom" -> s"${S2Graph.KWGResource}geometry.polygon.s2.",
    "rdf" -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdfs" -> "http://www.w3.org/2000/01/rdf-schema#",
    "spatial-full" -> S2Graph.SpatialFull,
    "void" -> "http://rdfs.org/ns/void#",
    "xsd" -> "http://www.w3.org/2001/XMLSchema#"
  )

  val PrefixMap: Map[String, String] = Prefixes.toMap

  /** The base every emitted link carries. The router is mounted at `/` and strips nothing, so a gateway that routes a prefix here removes
    * it before the request arrives and this puts it back on the way out (KGF doc 03 section 3.2).
    */
  val PublicBase: Uri =
    sys.env
      .get("KGF_PUBLIC_BASE")
      .flatMap(value => Uri.parse(value.stripSuffix("/")).toOption)
      .getOrElse(uri"http://localhost:8080")

  private val basePath: String =
    val path = PublicBase.pathSegments.segments.map(_.v).filter(_.nonEmpty).mkString("/")
    if path.isEmpty then "" else s"/$path"

  /** A root-relative path under the public base, which is the form JSON descriptor links take (KGF doc 03 section 3.2) */
  def link(path: String): String = s"$basePath$path"

  def absolute(path: String): String = s"${PublicBase.toString.stripSuffix("/")}$path"

  val DatasetPath = s"/$DatasetId"

  val VersionPath = s"$DatasetPath/v/$Version"

  def operationPath(operation: String): String = s"$VersionPath/$operation"

  /** The operations a client reaches from a release, in the order the descriptor lists them */
  val Operations = Vector("manifest", "fragment", "tpf", "count", "describe", "graphs", "terms", "schema", "void", "summary")

  /** The digest of the code this process runs, as `sha256:` and 64 hex characters.
    *
    * Taken over every entry of the classpath, each by name and content, so a change to the S2 library or to Jena changes it as surely as a
    * change to this server does: all three decide the answers. `KGF_CONTENT_DIGEST` overrides it where the deployment knows the container
    * image digest, which is the artifact KGF doc 05 section 5.8 names as a computed graph's release.
    */
  lazy val ContentDigest: String =
    sys.env.get("KGF_CONTENT_DIGEST").filter(_.nonEmpty).getOrElse(s"sha256:$classpathDigest")

  private def classpathDigest: String =
    val digest = MessageDigest.getInstance("SHA-256")
    val entries = Option(System.getProperty("java.class.path")).getOrElse("").split(java.io.File.pathSeparatorChar).toVector.sorted
    entries.foreach { entry =>
      digest.update(entry.getBytes("UTF-8"))
      Try(Paths.get(entry)).toOption.filter(Files.exists(_)).foreach(path => hash(path, digest))
    }
    hex(digest.digest())

  private def hash(path: Path, digest: MessageDigest): Unit =
    if Files.isDirectory(path) then
      Using(Files.walk(path)) { walk =>
        walk.iterator().asScala.filter(Files.isRegularFile(_)).toVector.map(file => path.relativize(file).toString -> file).sortBy(_._1)
      }.toOption.toVector.flatten.foreach { (name, file) =>
        digest.update(name.getBytes("UTF-8"))
        Try(digest.update(Files.readAllBytes(file)))
      }
    else Try(digest.update(Files.readAllBytes(path)))

  private def hex(bytes: Array[Byte]): String = bytes.map(byte => f"${byte & 0xff}%02x").mkString
