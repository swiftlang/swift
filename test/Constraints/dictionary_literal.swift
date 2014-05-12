// RUN: %swift -parse -verify %s

class DictStringInt : DictionaryLiteralConvertible {
  typealias Key = String
  typealias Value = Int
  class func convertFromDictionaryLiteral(elements: (String, Int)...) -> DictStringInt {}
}

class Dictionary<K, V> : DictionaryLiteralConvertible {
  typealias Key = K
  typealias Value = V
  class func convertFromDictionaryLiteral(elements: (K, V)...) -> Dictionary<K, V> {}
}

func useDictStringInt(d: DictStringInt) {}
func useDict<K, V>(d: Dictionary<K,V>) {}

// Concrete dictionary literals.
useDictStringInt([ "Hello" : 1 ])
useDictStringInt([ "Hello" : 1, "World" : 2])
useDictStringInt([ "Hello" : 1, "World" : 2.5]) // expected-error{{}}
useDictStringInt([ 7 : 1, "World" : 2]) // expected-error{{}}

// Generic dictionary literals.
useDict(["Hello" : 1])
useDict(["Hello" : 1, "World" : 2])
useDict(["Hello" : 1.5, "World" : 2])
useDict([1 : 1.5, 3 : 2.5])

// Fall back to Dictionary<K, V> if no context is otherwise available.
var a = ["Hello" : 1, "World" : 2]
var a2 : Dictionary<String, Int> = a
var a3 = ["Hello" : 1]

var b = [ 1 : 2, 1.5 : 2.5 ]
var b2 : Dictionary<Double, Double> = b
var b3 = [1 : 2.5]
