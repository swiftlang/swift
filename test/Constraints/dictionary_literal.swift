// RUN: %swift -parse -verify -constraint-checker %s

protocol DictionaryLiteralConvertible {
  typealias Key
  typealias Value
  static func convertFromDictionaryLiteral(xs:(Key, Value)...) -> This
}

class DictStringInt : DictionaryLiteralConvertible {
  typealias Key = String
  typealias Value = Int
  static func convertFromDictionaryLiteral(xs:(String, Int)...) -> DictStringInt {}
}

/* FIXME: The associated/self type of a generic type fails to conform to
 * protocols <rdar://problem/13153805>, so the explicit conformances below
 * are disabled. */

class Dictionary<K, V> /*: DictionaryLiteralConvertible*/ {
  typealias Key = K
  typealias Value = V
  static func convertFromDictionaryLiteral(xs:(K, V)...) -> Dictionary<K, V> {}
}

func useDictStringInt(d:DictStringInt) {}
func useDict<K,V>(d:Dictionary<K,V>) {}

// Concrete dictionary literals.
useDictStringInt([ "Hello" : 1, "World" : 2])
useDictStringInt([ "Hello" : 1, "World" : 2.5]) // expected-error{{}}
useDictStringInt([ 7 : 1, "World" : 2]) // expected-error{{}}

// Generic dictionary literals.
useDict(["Hello" : 1, "World" : 2])
useDict(["Hello" : 1.5, "World" : 2])
useDict([1 : 1.5, 3 : 2.5])

// Fall back to Dictionary<K, V> if no context is otherwise available.
var a = ["Hello" : 1, "World" : 2]
var a2 : Dictionary<String, Int> = a

var b = [ 1 : 2, 1.5 : 2.5 ]
var b2 : Dictionary<Double, Double> = b
