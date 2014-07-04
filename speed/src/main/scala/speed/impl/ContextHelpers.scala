/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2013-2014 Johannes Rudolph
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package speed.impl

trait ContextHelpers { self: WithContext ⇒
  import c.universe._

  lazy val IntTag = c.weakTypeOf[Int]
  lazy val LongTag = c.weakTypeOf[Long]
  lazy val FloatTag = c.weakTypeOf[Float]
  lazy val DoubleTag = c.weakTypeOf[Double]
  lazy val ShortTag = c.weakTypeOf[Short]
  lazy val ByteTag = c.weakTypeOf[Byte]
  lazy val BooleanTag = c.weakTypeOf[Boolean]
  lazy val CharTag = c.weakTypeOf[Char]
  def neutralElement[T: c.WeakTypeTag]: Tree = neutralElement(c.universe.weakTypeOf[T])
  def neutralElement(tpe: Type): Tree =
    lit(tpe match {
      case IntTag     ⇒ 0
      case LongTag    ⇒ 0L
      case FloatTag   ⇒ 0f
      case DoubleTag  ⇒ 0d
      case ShortTag   ⇒ 0.toShort
      case ByteTag    ⇒ 0.toByte
      case BooleanTag ⇒ false
      case CharTag    ⇒ 0.toChar
      case _          ⇒ null
    })
  def lit(v: Any) = Literal(Constant(v))
}
