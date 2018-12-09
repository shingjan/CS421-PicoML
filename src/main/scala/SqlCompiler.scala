import scala.lms.common._

trait QueryAST {
  type Table
  type Schema = Vector[String]

  // relational algebra ops
  sealed abstract class Operator
  case class Scan(name: Table, schema: Schema, delim: Char, extSchema: Boolean) extends Operator
  case class PrintCSV(parent: Operator) extends Operator
  case class Project(outSchema: Schema, inSchema: Schema, parent: Operator) extends Operator
  case class Filter(pred: Predicate, parent: Operator) extends Operator
  case class Join(parent1: Operator, parent2: Operator) extends Operator
  case class Group(keys: Schema, agg: Schema, parent: Operator) extends Operator
  case class HashJoin(parent1: Operator, parent2: Operator) extends Operator

  // filter predicates
  sealed abstract class Predicate
  case class Eq(a: Ref, b: Ref) extends Predicate

  sealed abstract class Ref
  case class Field(name: String) extends Ref
  case class Value(x: Any) extends Ref

  // some smart constructors
  def Schema(schema: String*): Schema = schema.toVector
  def Scan(tableName: String): Scan = Scan(tableName, None, None)
  def Scan(tableName: String, schema: Option[Schema], delim: Option[Char]): Scan
}


/**
SQL Parser
----------
We add a parser that takes a SQL(-like) string and converts it to tree of operators.
  */

trait SQLParser extends QueryAST {
  import scala.util.parsing.combinator._

  def parseSql(input: String) = Grammar.parseAll(input)

  object Grammar extends JavaTokenParsers with PackratParsers {

    def stm: Parser[Operator] =
      selectClause ~ fromClause ~ whereClause ~ groupClause ^^ {
        case p ~ s ~ f ~ g => g(p(f(s))) }
    def selectClause: Parser[Operator=>Operator] =
      "select" ~> ("*" ^^ { _ => (op:Operator) => op } | fieldList ^^ {
        case (fs,fs1) => Project(fs,fs1,_:Operator) })
    def fromClause: Parser[Operator] =
      "from" ~> joinClause
    def whereClause: Parser[Operator=>Operator] =
      opt("where" ~> predicate ^^ { p => Filter(p, _:Operator) }) ^^ { _.getOrElse(op => op)}
    def groupClause: Parser[Operator=>Operator] =
      opt("group" ~> "by" ~> fieldIdList ~ ("sum" ~> fieldIdList) ^^ {
        case p1 ~ p2 => Group(p1,p2, _:Operator) }) ^^ { _.getOrElse(op => op)}

    def joinClause: Parser[Operator] =
      ("nestedloops" ~> repsep(tableClause, "join") ^^ { _.reduceLeft((a,b) => Join(a,b)) }) |
        (repsep(tableClause, "join") ^^ { _.reduceLeft((a,b) => HashJoin(a,b)) })
    def tableClause: Parser[Operator] =
      tableIdent ~ opt("schema" ~> fieldIdList) ~
        opt("delim" ~> ("""\t""" ^^ (_ => '\t') | """.""".r ^^ (_.head))) ^^ {
        case table ~ schema ~ delim => Scan(table, schema, delim) } |
        ("(" ~> stm <~ ")")

    def fieldIdent: Parser[String] = """[\w\#]+""".r
    def tableIdent: Parser[String] = """[\w_\-/\.]+""".r | "?"
    def fieldList:  Parser[(Schema,Schema)] =
      repsep(fieldIdent ~ opt("as" ~> fieldIdent), ",") ^^ { fs2s =>
        val (fs,fs1) = fs2s.map { case a~b => (b.getOrElse(a),a) }.unzip
        (Schema(fs:_*),Schema(fs1:_*)) }
    def fieldIdList:  Parser[Schema] =
      repsep(fieldIdent,",") ^^ (fs => Schema(fs:_*))

    def predicate: Parser[Predicate] =
      ref ~ "=" ~ ref ^^ { case a ~ _ ~ b => Eq(a,b) }
    def ref: Parser[Ref] =
      fieldIdent ^^ Field |
        """'[^']*'""".r ^^ (s => Value(s.drop(1).dropRight(1))) |
        """[0-9]+""".r ^^ (s => Value(s.toInt))

    def parseAll(input: String): Operator = parseAll(stm,input) match {
      case Success(res,_)  => res
      case res => throw new Exception(res.toString)
    }
  }
}

trait QueryProcessor extends QueryAST {
  def version: String
  val defaultFieldDelimiter = ','

  def filePath(table: String): String = table
  def dynamicFilePath(table: String): Table

  def Scan(tableName: String, schema: Option[Schema], delim: Option[Char]): Scan = {
    val dfile = dynamicFilePath(tableName)
    val (schema1, externalSchema) = schema.map(s=>(s,true)).getOrElse((loadSchema(filePath(tableName)),false))
    Scan(dfile, schema1, delim.getOrElse(defaultFieldDelimiter), externalSchema)
  }

  def loadSchema(filename: String): Schema = {
    val s = new Scanner(filename)
    val schema = Schema(s.next('\n').split(defaultFieldDelimiter): _*)
    s.close
    schema
  }

  def execQuery(q: Operator): Unit
}

trait PlainQueryProcessor extends QueryProcessor {
  type Table = String
}

trait StagedQueryProcessor extends QueryProcessor with Dsl {
  type Table = Rep[String] // dynamic filename
  override def filePath(table: String) = if (table == "?") throw new Exception("file path for table ? not available") else super.filePath(table)
}

trait Engine extends QueryProcessor with SQLParser {
  def query: String
  def filename: String
  def liftTable(n: String): Table
  def eval: Unit
  def prepare: Unit = {}
  def run: Unit = execQuery(PrintCSV(parseSql(query)))
  override def dynamicFilePath(table: String): Table =
    liftTable(if (table == "?") filename else filePath(table))
  def evalString = {
    val source = new java.io.ByteArrayOutputStream()
    utils.withOutputFull(new java.io.PrintStream(source)) {
      eval
    }
    source.toString
  }
}
trait StagedEngine extends Engine with StagedQueryProcessor {
  override def liftTable(n: String) = unit(n)
}

object Run {
  var qu: String = _
  var fn: String = _

  trait MainEngine extends Engine {
    override def query = qu
    override def filename =  fn
  }

  def unstaged_engine: Engine =
    new Engine with MainEngine with query_unstaged.QueryInterpreter {
      override def liftTable(n: Table) = n
      override def eval = run
    }

  def scala_engine =
    new DslDriver[String,Unit] with ScannerExp
      with StagedEngine with MainEngine with query_staged.QueryCompiler { q =>
      override val codegen = new DslGen with ScalaGenScanner {
        val IR: q.type = q
      }
      override def snippet(fn: Table): Rep[Unit] = run
      override def prepare: Unit = precompile
      override def eval: Unit = eval(filename)
    }

  def c_engine =
    new DslDriverC[String,Unit] with ScannerLowerExp
      with StagedEngine with MainEngine with query_optc.QueryCompiler { q =>
      override val codegen = new DslGenC with CGenScannerLower {
        val IR: q.type = q
      }
      override def snippet(fn: Table): Rep[Unit] = run
      override def prepare: Unit = {}
      override def eval: Unit = eval(filename)
    }

  def main(args: Array[String]) {
    if (args.length < 2) {
      println("Wrong syntax for SQL statement.")
      return
    }
    val version = args(0)
    val engine = version match {
      case "unstaged" => unstaged_engine
      case "c" => c_engine
      case "scala" => scala_engine
      case _ => println("warning: unexpected engine, using 'unstaged' by default")
        unstaged_engine
    }
    qu = args(1)
    if (args.length > 2)
      fn = args(2)

    try {
      engine.prepare
      utils.time(engine.eval)
    } catch {
      case ex: Exception =>
        println("ERROR: " + ex)
    }
  }
}
