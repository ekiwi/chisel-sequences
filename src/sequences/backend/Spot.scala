package sequences.backend

import chisel3.Bool

import scala.collection.mutable

object Spot {
  private class Serializer() {
    val nameMap = mutable.Map[Bool, String]()
    var nextChar: Char = 'a'

    def toPSL(s: Sequence): String = {
      s match {
        case SeqPred(predicate) =>
          /*
          predicate.litToBooleanOption match {
            case Some(true) => "(t)"
            case Some(false) => "(f)"
            case None =>
              nameMap.getOrElseUpdate(predicate, {
                val c = nextChar.toString;
                nextChar = (nextChar + 1).toChar;
                s"($c)"
              })
          }
           */
          s"($predicate)"
        case SeqOr(s1, s2) => ???
        case SeqConcat(s1, s2) =>
          s"(${toPSL(s1)} & X(${toPSL(s2)}))"
        case SeqIntersect(s1, s2)   => ???
        case SeqNot(s1)             => ???
        case SeqImplies(s1, p1)     => ???
        case SeqImpliesNext(s1, p1) => ???
        case SeqFuse(s1, s2)        => ???
      }
    }
  }

  def toPSL(s: Sequence): String = {
    val ser = new Serializer()
    ser.toPSL(s)
  }

  def callSpot(psl: String): String = {
    val cmd = Seq("ltl2tgba", "-B", "-D", "-f", psl)
    val result = os.proc(cmd).call()
    assert(result.exitCode == 0)
    // val inputStream = new ByteArrayInputStream(result.out.bytes)
    // val bufferedInputStream = new BufferedInputStream(inputStream)
    //val consumer = new HOAParserConsumer()
    //HOAFParser.parseHOA(bufferedInputStream, consumer)
    //bufferedInputStream.close()
    //inputStream.close()
    //consumer.partialDeterministic()
    //consumer.addAuxVar()
    //BVSymbol(p.name, bitWidth(p.tpe).toInt)
    //  println(s"auxVarNum: ${h.auxVarNum}")
    //  println(s"apNum: ${h.apNum}")
    result.out.toString
  }

  /*
    def compile(p: Property): PropertyIO = {
      p match {
        case PropSeq(s) =>
      }
    }
   */
}
