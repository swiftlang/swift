// RUN: %swift -parse -verify %s

class Dictionary<K, V> : DictionaryLiteralConvertible { // expected-error {{type 'Dictionary<K, V>' does not conform to protocol 'DictionaryLiteralConvertible'}}
  typealias Key = K
  typealias Value = V
  func convertFromDictionaryLiteral(xs: (K, V)...) -> Dictionary<K, V> {} // expected-note {{candidate operates on an instance, not a type as required}}
}

func useDict<K, V>(d: Dictionary<K,V>) {}

useDict(["Hello" : 1])

