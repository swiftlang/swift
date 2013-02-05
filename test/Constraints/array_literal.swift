// RUN: %swift -parse -verify -constraint-checker %s

protocol ArrayLiteralConvertible {
  typealias Element
  static func convertFromArrayLiteral(xs:Element...) -> This
}

class IntList : ArrayLiteralConvertible {
  typealias Element = Int
  static func convertFromArrayLiteral(xs:Int...) -> IntList {}
}

class DoubleList : ArrayLiteralConvertible {
  typealias Element = Double
  static func convertFromArrayLiteral(xs:Double...) -> DoubleList {}
}

class IntDict : ArrayLiteralConvertible {
  typealias Element = (String, Int)
  static func convertFromArrayLiteral(xs:Element...) -> IntDict {}
}

class DoubleDict : ArrayLiteralConvertible {
  typealias Element = (String, Double)
  static func convertFromArrayLiteral(xs:Element...) -> DoubleDict {}
}

/* FIXME: The associated/self type of a generic type fails to conform to
 * protocols <rdar://problem/13153805>, so the explicit conformances below
 * are disabled. */

class List<T> /*: ArrayLiteralConvertible*/ {
  typealias Element = T
  static func convertFromArrayLiteral(xs:T...) -> List<T> {}
}

class Dict<K,V> /*: ArrayLiteralConvertible*/ {
  typealias Element = (K,V)

  static func convertFromArrayLiteral(xs:(K,V)...) -> Dict<K,V> {}
}

func [infix] => <K,V>(k:K, v:V) -> (K,V) { return (k,v) }

func useIntList(l:IntList) {}
func useDoubleList(l:DoubleList) {}
func useIntDict(l:IntDict) {}
func useDoubleDict(l:DoubleDict) {}
func useList<T>(l:List<T>) {}
func useDict<K,V>(d:Dict<K,V>) {}

useIntList([1,2,3])
useIntList([1.0,2,3]) // expected-error{{}}
useDoubleList([1.0,2,3])
useDoubleList([1.0,2.0,3.0])

useIntDict(["Niners" => 31, "Ravens" => 34])
useIntDict(["Niners" => 31, "Ravens" => 34.0]) // expected-error{{}}
useDoubleDict(["Niners" => 31, "Ravens" => 34.0])
useDoubleDict(["Niners" => 31.0, "Ravens" => 34.0])

/* FIXME The constraint solver can't quite handle the systems for these yet.
useList([1,2,3])
useList([1.0,2,3])
useList([1.0,2.0,3.0])
useDict(["Niners" => 31, "Ravens" => 34])
useDict(["Niners" => 31, "Ravens" => 34.0])
useDict(["Niners" => 31.0, "Ravens" => 34.0])
*/

/* FIXME
// Should fall back to T[] type if no context is otherwise available

var a = [1,2,3]
var a2 : Int[] = a

var b = [1,2,3.0]
var b2 : Double[] = b
*/
