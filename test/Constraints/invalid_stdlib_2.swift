// RUN: %target-parse-verify-swift

class Dictionary<K, V> : DictionaryLiteralConvertible { // expected-error {{type 'Dictionary<K, V>' does not conform to protocol 'DictionaryLiteralConvertible'}}
  typealias Key = K
  typealias Value = V
  init(dictionaryLiteral xs: (K)...){} // expected-note {{}}
}

func useDict<K, V>(d: Dictionary<K,V>) {}

useDict(["Hello" : 1])
useDict(["Hello" : 1, "World" : 2])

