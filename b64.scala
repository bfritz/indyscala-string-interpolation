import com.google.common.base.Charsets.US_ASCII
import com.google.common.io.BaseEncoding.base64

// Based on Scala's StringContext class:
// https://github.com/scala/scala/blob/v2.10.0/src/library/scala/StringContext.scala
implicit class Base64String(val sc: StringContext) extends AnyVal {
  def b64(args: String*): String = {

    if (sc.parts.length != args.length + 1)
      throw new IllegalArgumentException(
      "wrong number of arguments for b64 interpolated string")

    val strings = sc.parts.iterator
    val expressions = args.iterator
    val bldr = new java.lang.StringBuilder(strings.next)
    while (expressions.hasNext) {
      bldr append strings.next
      bldr append base64().encode(expressions.next.getBytes(US_ASCII))
    }
    bldr.toString
  }
}
