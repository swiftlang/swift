// RUN: %swift -parse -verify -constraint-checker %s
// XFAIL: *

protocol ArrayLiteralConvertible {
  typealias Element
  static func convertFromArrayLiteral(xs:Element...) -> This
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

func useList<T>(l:List<T>) {}
func useDict<K,V>(d:Dict<K,V>) {}

useList([1,2,3])
useList([1.0,2,3])
useList([1.0,2.0,3.0])
useDict(["Niners" => 31, "Ravens" => 34])
useDict(["Niners" => 31, "Ravens" => 34.0])
useDict(["Niners" => 31.0, "Ravens" => 34.0])
