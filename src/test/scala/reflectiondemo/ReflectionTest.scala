package reflectiondemo

import java.time.OffsetDateTime

import org.scalatest.path

/**
  * Created by dnwiebe on 11/26/16.
  */
class ReflectionTest extends path.FunSpec {

  describe ("Given a class") {
    val cls = classOf[SRExample]

    describe ("we can get a Constructor object") {
      val constructor = cls.getConstructor (classOf [String], classOf [Int], classOf [Double])

      describe ("which can be used to construct an instance") {
        val instance = constructor.newInstance ("Booga", 74.asInstanceOf[java.lang.Integer],
          4.321.asInstanceOf[java.lang.Double]).asInstanceOf[SRExample]

        it ("that has the supplied values") {
          assert (instance.ctorParam1 === "Booga")
          assert (instance.ctorParam2 === 74)
          assert (instance.getTypeParam === 4.321)
        }

        describe ("we can get a Method object") {
          val method = cls.getMethod ("ctorParam1")

          describe ("which can be used to call a public method") {
            val result = method.invoke (instance)

            assert (result === "Booga")
          }
        }

        describe ("we can get a declared Method object") {
          val method = cls.getDeclaredMethod ("ctorParam2")

          describe ("which can be used to call a private method after modifying the accessibility") {
            method.setAccessible (true)
            val result = method.invoke (instance)

            assert (result === 74)
          }
        }

        describe ("we can get a Field object") {
          val field = cls.getDeclaredField("publicField")
          field.setAccessible (true)

          describe ("which can be used to set a public field") {
            field.set(instance, 12.2)

            it ("as can be verified by checking") {
              assert (instance.publicField === 12.2)
            }
          }
        }

        describe ("we can get a Field object") {
          val field = cls.getDeclaredField("privateField")
          field.setAccessible (true)

          describe ("which can be used to set a private field") {
            field.set(instance, false)

            it ("as can be verified by checking through reflection") {
              assert (field.get (instance) === false)
            }
          }
        }
      }
    }

    it ("we cannot get static fields or methods because they don't exist in Scala") {
      try {
        cls.getDeclaredField ("message")
      }
      catch {
        case e: NoSuchFieldException => assert (e.getMessage === "message")
      }
    }
  }

  describe ("We can get the companion class") {
    val companion = classOf[SRExample.type]

    it ("but can't get its instance") {
      assert (companion != null)
    }
  }
}
