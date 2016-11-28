package reflectiondemo

import org.scalatest.path

import scala.reflect.runtime.universe._

class ListHolder[T] (first: T, second: T) {
  val genericList: List[T] = List (first, second)
  val specificList: List[Int] = List (1, 2, 3)
}

class IntrospectionTest extends path.FunSpec {

  describe ("Given a ListHolder object") {
    val classloaderMirror = runtimeMirror(getClass.getClassLoader)
    val listHolder = new ListHolder ("one", "two")

    describe ("and a bunch of reflective machinery around it") {
      val classSymbol = symbolOf[ListHolder[String]].asClass
      val typeParams = classSymbol.typeSignature.typeParams
      assert (typeParams.head.fullName.toString === "reflectiondemo.ListHolder.T")


      val instanceMirror = classloaderMirror.reflect(listHolder)
      val genericListSymbol = weakTypeTag[ListHolder[_]].tpe.decl(TermName ("genericList")).asTerm
      val genericListMirror = instanceMirror.reflectField (genericListSymbol)
      val specificListSymbol = weakTypeTag[ListHolder[_]].tpe.decl(TermName ("specificList")).asTerm
      val specificListMirror = instanceMirror.reflectField (specificListSymbol)

      it ("the full type of the specific list can be retrieved") {
        assert (specificListMirror.symbol.typeSignature === typeOf[List[Int]])
      }

      it ("the name of the generic type argument can be retrieved, but not the dynamic type") {
        val info = genericListMirror.symbol.info
        assert (info.typeArgs.head.toString === "T")
      }
    }
  }

  describe ("Given a type for a class") {
    val classType = typeOf[SRExample]

    it ("we can get a lot of information from the type") {
      assert (classType.baseClasses === List(
        symbolOf[SRExample],
        symbolOf[Object],
        symbolOf[Any]
      ))
      assert (classType.baseType (symbolOf[Object]) === typeTag[Object].tpe)
      assert (classType.contains (symbolOf[Object]) === false)
      assert (classType.dealias === classType)
      assert (classType.ensuring (t => true) === classType)
      assert (classType.decl (TermName("ctorParam1")).fullName === "reflectiondemo.SRExample.ctorParam1")
      assert (classType.companion.toString === "reflectiondemo.SRExample.type")
      assert (classType.erasure.toString === "reflectiondemo.SRExample")
      assert (classType.etaExpand.toString === "reflectiondemo.SRExample")
      assert (classType.exists (t => true) === true)
      assert (classType.finalResultType === classType)
      assert (classType.find (t => true) === Some(classType))
      assert (classType.map (t => typeOf[String]) === typeOf[String])
      assert (classType.member (TermName("ctorParam1")).toString === "value ctorParam1")
      assert (classType.members.toString ===
        "Scope{\n" +
        "  def getTypeParam: <?>;\n" +
        "  def privateMethod(param1: Int,param2: String): java.time.OffsetDateTime;\n" +
        "  def publicMethod(param1: String,param2: Int): Boolean;\n" +
        "  private[this] val publicField: Double;\n" +
        "  val publicField: <?>;\n" +
        "  private[this] val privateField: Boolean;\n" +
        "  private val privateField: <?>;\n" +
        "  def <init>(ctorParam1: String,ctorParam2: Int,typeParam: Double): reflectiondemo.SRExample;\n" +
        "  private[this] val typeParam: <?>;\n" +
        "  private[this] val ctorParam2: <?>;\n" +
        "  val ctorParam2: <?>;\n" +
        "  private[this] val ctorParam1: <?>;\n" +
        "  val ctorParam1: <?>;\n" +
        "  final def $asInstanceOf[T0](): T0;\n" +
        "  final def $isInstanceOf[T0](): Boolean;\n" +
        "  final def synchronized[T0](x$1: T0): T0;\n" +
        "  final def ##(): Int;\n" +
        "  final def !=(x$1: Any): Boolean;\n" +
        "  final def ==(x$1: Any): Boolean;\n" +
        "  final def ne(x$1: AnyRef): Boolean;\n" +
        "  final def eq(x$1: AnyRef): Boolean;\n" +
        "  final def notifyAll(): Unit;\n" +
        "  final def notify(): Unit;\n" +
        "  protected[package lang] def clone(): java.lang.Object;\n" +
        "  final def getClass(): java.lang.Class[_];\n" +
        "  def hashCode(): Int;\n" +
        "  def toString(): java.lang.String;\n" +
        "  def equals(x$1: Any): Boolean;\n" +
        "  final def wait(): Unit;\n" +
        "  final def wait(x$1: Long): Unit;\n" +
        "  final def wait(x$1: Long,x$2: Int): Unit;\n" +
        "  protected[package lang] def finalize(): Unit;\n" +
        "  final def asInstanceOf[T0]: T0;\n" +
        "  final def isInstanceOf[T0]: Boolean\n" +
        "}"
      )
      assert (classType.orElse (null) === classType)
      assert (classType.paramLists === Nil)
      assert (classType.resultType === classType)
      assert (classType.takesTypeArgs === false)
      assert (classType.termSymbol === NoSymbol)
      assert (classType.typeArgs === Nil)
      assert (classType.typeConstructor === classType)
      assert (classType.typeParams === Nil)
      assert (classType.typeSymbol === symbolOf[SRExample])
      assert (classType.widen === classType)
      assert (classType.=:= (classType) === true)
      assert (classType.<:< (classType) === true)
      assert (classType.weak_<:< (classType) === true)
    }
  }
}
