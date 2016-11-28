package reflectiondemo

import java.time.OffsetDateTime
import org.scalatest.path
import scala.reflect.runtime.universe._

object SRExample {
  val message = "I'm in a companion object!"
}

class SRExample (val ctorParam1: String, val ctorParam2: Int, typeParam: Double) {
  private val privateField = true
  val publicField = 10.0

  def publicMethod (param1: String, param2: Int): Boolean = {
    false
  }

  def privateMethod (param1: Int, param2: String): OffsetDateTime = {
    OffsetDateTime.MIN
  }

  def getTypeParam: Double = typeParam
}

/**
  * Created by dnwiebe on 11/15/16.
  */
class MirrorTest extends path.FunSpec {

  describe ("Given a classloader mirror") {
    val classloaderMirror = runtimeMirror(getClass.getClassLoader)

    describe ("it can be used to produce a class mirror") {
      val classSymbol = symbolOf[SRExample].asClass
      val classMirror = classloaderMirror.reflectClass(classSymbol)

      describe ("which can be used to produce a constructor (method) mirror") {
        val constructorSymbol = typeOf[SRExample].decl(termNames.CONSTRUCTOR).asMethod
        val constructorMirror = classMirror.reflectConstructor(constructorSymbol)

        describe ("which can be used to construct an instance") {
          val instance = constructorMirror("Booga", 74, 4.321).asInstanceOf[SRExample]

          it ("that has the supplied values") {
            assert (instance.ctorParam1 === "Booga")
            assert (instance.ctorParam2 === 74)
            assert (instance.getTypeParam === 4.321)
          }

          describe ("that, when handed to the classloader mirror, can make an instance mirror") {
            val instanceMirror = classloaderMirror.reflect (instance)

            describe ("that can be used to call a public method") {
              val methodSymbol = typeOf[SRExample].decl(TermName ("publicMethod")).asMethod
              val methodMirror = instanceMirror.reflectMethod (methodSymbol)
              val result = methodMirror ("string", 1)

              it ("which returns the expected value") {
                assert (result === false)
              }
            }

            describe ("that can be used to call a private method") {
              val methodSymbol = typeOf[SRExample].decl(TermName ("privateMethod")).asMethod
              val methodMirror = instanceMirror.reflectMethod (methodSymbol)
              val result = methodMirror (1, "string")

              it ("which returns the expected value") {
                assert (result === OffsetDateTime.MIN)
              }
            }

            describe ("that can be used to get a field mirror for a public field") {
              val fieldSymbol = typeOf[SRExample].decl (TermName ("publicField")).asTerm
              val fieldMirror = instanceMirror.reflectField (fieldSymbol)

              describe ("that can get the value of the field") {
                val result = fieldMirror.get.asInstanceOf [Double]

                it ("which has the expected value") {
                  assert (result === 10.0)
                }
              }

              describe ("that can set the value of the field, even though it's supposedly immutable") {
                fieldMirror.set (12.34)

                it ("as can be seen in the object itself") {
                  assert (instance.publicField === 12.34)
                }
              }
            }

            describe ("that can be used to get a field mirror for a private field") {
              val fieldSymbol = typeOf[SRExample].decl (TermName ("privateField")).asTerm
              val fieldMirror = instanceMirror.reflectField (fieldSymbol)

              describe ("that can get the value of the field") {
                val result = fieldMirror.get.asInstanceOf [Boolean]

                it ("which has the expected value") {
                  assert (result === true)
                }
              }

              describe ("that can set the value of the field, even though it's supposedly immutable") {
                fieldMirror.set (false)

                it ("as can be seen using the mirror again") {
                  assert (fieldMirror.get === false)
                }
              }
            }
          }
        }
      }
    }

    describe ("it can be used to produce a module mirror") {
      val singletonSymbol = typeOf[SRExample.type].termSymbol.asModule
      val moduleMirror = classloaderMirror.reflectModule (singletonSymbol)
      val singleton = moduleMirror.instance.asInstanceOf[SRExample.type]

      it ("...see?") {
        assert (singleton.message === "I'm in a companion object!")
      }
    }
  }
}

