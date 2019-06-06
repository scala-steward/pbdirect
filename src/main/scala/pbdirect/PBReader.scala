/*
 * Copyright (c) 2019 Beyond the lines
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

package pbdirect

import java.io.ByteArrayOutputStream

import cats.Functor
import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}

import scala.util.Try

trait PBReader[A] {
  def read(input: CodedInputStream): Option[A]
}
trait LowerPriorityPBReaderImplicits {
  def instance[A](f: (CodedInputStream) => Option[A]): PBReader[A] =
    new PBReader[A] {
      override def read(input: CodedInputStream): Option[A] = f(input)
    }
  implicit def coprodReader[A, R <: Coproduct](
      implicit
      gen: Generic.Aux[A, R],
      repr: Lazy[PBParser[R]]): PBReader[A] = instance { (input: CodedInputStream) =>
    val bytes = input.readByteArray()

    // wraps the bytes into a protobuf single field message
    val out   = new ByteArrayOutputStream()
    val pbOut = CodedOutputStream.newInstance(out)
    pbOut.writeByteArray(1, bytes)
    pbOut.flush()

    repr.value.parse(1, out.toByteArray).map(gen.from)
  }
}
trait PBReaderImplicits extends LowerPriorityPBReaderImplicits {

  implicit def prodReader[A, R <: HList](
      implicit
      gen: Generic.Aux[A, R],
      repr: Lazy[PBParser[R]]): PBReader[A] = instance { input: CodedInputStream =>
    val bytes = input.readByteArray()
    repr.value.parse(1, bytes).map(gen.from)
  }

  implicit def enumReader[A](
      implicit
      values: Enum.Values[A],
      ordering: Ordering[A],
      reader: PBReader[Int]): PBReader[A] = instance { input: CodedInputStream =>
    reader.read(input).map(Enum.fromInt[A](_))
  }
  implicit def enumerationReader[E <: Enumeration](
      implicit
      reader: PBReader[Int],
      gen: Generic.Aux[E, HNil]): PBReader[E#Value] = instance { input: CodedInputStream =>
    val enum = gen.from(HNil)
    reader.read(input).map(enum(_))
  }
}
object PBReader extends PBReaderImplicits {
  implicit object BooleanReader$ extends PBReader[Boolean] {
    override def read(input: CodedInputStream): Option[Boolean] = Try(input.readBool()).toOption
  }
  implicit object IntReader$ extends PBReader[Int] {
    override def read(input: CodedInputStream): Option[Int] = Try(input.readInt32()).toOption
  }
  implicit object LongReader$ extends PBReader[Long] {
    override def read(input: CodedInputStream): Option[Long] = Try(input.readInt64()).toOption
  }
  implicit object FloatReader$ extends PBReader[Float] {
    override def read(input: CodedInputStream): Option[Float] = Try(input.readFloat()).toOption
  }
  implicit object DoubleReader$ extends PBReader[Double] {
    override def read(input: CodedInputStream): Option[Double] = Try(input.readDouble()).toOption
  }
  implicit object StringReader$ extends PBReader[String] {
    override def read(input: CodedInputStream): Option[String] = Try(input.readString()).toOption
  }
  implicit object BytesReader$ extends PBReader[Array[Byte]] {
    override def read(input: CodedInputStream): Option[Array[Byte]] =
      Try(input.readByteArray()).toOption
  }

  def apply[A: PBReader]: PBReader[A] = implicitly

  implicit object FunctorReader extends Functor[PBReader] {
    override def map[A, B](reader: PBReader[A])(f: A => B): PBReader[B] = instance {
      input: CodedInputStream =>
        reader.read(input).map(f)
    }
  }
}

trait PBParser[A] {
  def parse(index: Int, bytes: Array[Byte]): Option[A]
}

trait LowPriorityPBParserImplicits {
  def instance[A](f: (Int, Array[Byte]) => Option[A]): PBParser[A] = new PBParser[A] {
    override def parse(index: Int, bytes: Array[Byte]): Option[A] = f(index, bytes)
  }
  implicit val hnilParser: PBParser[HNil] = instance { (index: Int, bytes: Array[Byte]) =>
    Some(HNil)
  }
  implicit def consParser[H, T <: HList](
      implicit
      head: PBParser[H],
      tail: Lazy[PBParser[T]]): PBParser[H :: T] = instance { (index: Int, bytes: Array[Byte]) =>
    for {
      h <- head.parse(index, bytes)
      t <- tail.value.parse(index + 1, bytes)
    } yield h :: t
  }

  implicit val cnilParser: PBParser[CNil] = instance { (index: Int, bytes: Array[Byte]) =>
    throw new UnsupportedOperationException("Can't read CNil")
  }
  implicit def cconsParser[H, T <: Coproduct](
      implicit
      head: PBParser[H],
      tail: Lazy[PBParser[T]]): PBParser[H :+: T] = instance { (index: Int, bytes: Array[Byte]) =>
    head
      .parse(index, bytes)
      .map(Inl(_))
      .orElse(tail.value.parse(index, bytes).map(Inr(_)))
  }
}

trait PBParserImplicits extends LowPriorityPBParserImplicits {
  implicit def repeatedParser[A](implicit reader: PBReader[A]): PBParser[List[A]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      val input       = CodedInputStream.newInstance(bytes)
      var done        = false
      var as: List[A] = Nil
      while (!done) {
        input.readTag() match {
          case 0                          => done = true
          case tag if (tag >> 3) == index => as :::= reader.read(input).toList
          case tag                        => Try(input.skipField(tag)).isSuccess
        }
      }
      Some(as.reverse)
    }
  implicit def requiredParser[A](implicit reader: PBReader[A]): PBParser[A] =
    instance { (index: Int, bytes: Array[Byte]) =>
      val input       = CodedInputStream.newInstance(bytes)
      var done        = false
      var as: List[A] = Nil
      while (!done) {
        input.readTag() match {
          case 0                          => done = true
          case tag if (tag >> 3) == index => as :::= reader.read(input).toList
          case tag                        => input.skipField(tag)
        }
      }
      as.headOption
    }
  implicit def optionalParser[A](implicit parser: PBParser[List[A]]): PBParser[Option[A]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      parser.parse(index, bytes).map(_.lastOption)
    }
  implicit def mapParser[K, V](implicit parser: PBParser[List[(K, V)]]): PBParser[Map[K, V]] =
    instance { (index: Int, bytes: Array[Byte]) =>
      parser.parse(index, bytes).map(_.toMap)
    }
}

object PBParser extends PBParserImplicits
