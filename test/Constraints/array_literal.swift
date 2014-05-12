// RUN: %swift -parse -verify %s

class IntList : ArrayLiteralConvertible {
  typealias Element = Int
  class func convertFromArrayLiteral(elements: Int...) -> IntList {}
}

class DoubleList : ArrayLiteralConvertible {
  typealias Element = Double
  class func convertFromArrayLiteral(elements: Double...) -> DoubleList {}
}

class IntDict : ArrayLiteralConvertible {
  typealias Element = (String, Int)
  class func convertFromArrayLiteral(elements: Element...) -> IntDict {}
}

class DoubleDict : ArrayLiteralConvertible {
  typealias Element = (String, Double)
  class func convertFromArrayLiteral(elements: Element...) -> DoubleDict {}
}

class List<T> : ArrayLiteralConvertible {
  typealias Element = T
  class func convertFromArrayLiteral(elements: T...) -> List<T> {}
}

class Dict<K,V> : ArrayLiteralConvertible {
  typealias Element = (K,V)

  class func convertFromArrayLiteral(elements: (K,V)...) -> Dict<K,V> {}
}

operator infix => {}

@infix func => <K, V>(k: K, v: V) -> (K,V) { return (k,v) }

func useIntList(l: IntList) {}
func useDoubleList(l: DoubleList) {}
func useIntDict(l: IntDict) {}
func useDoubleDict(l: DoubleDict) {}
func useList<T>(l: List<T>) {}
func useDict<K,V>(d: Dict<K,V>) {}

useIntList([1,2,3])
useIntList([1.0,2,3]) // expected-error{{}}
useDoubleList([1.0,2,3])
useDoubleList([1.0,2.0,3.0])

useIntDict(["Niners" => 31, "Ravens" => 34])
useIntDict(["Niners" => 31, "Ravens" => 34.0]) // expected-error{{}}
useDoubleDict(["Niners" => 31, "Ravens" => 34.0])
useDoubleDict(["Niners" => 31.0, "Ravens" => 34])
useDoubleDict(["Niners" => 31.0, "Ravens" => 34.0])

// Generic slices
useList([1,2,3])
useList([1.0,2,3])
useList([1.0,2.0,3.0])
useDict(["Niners" => 31, "Ravens" => 34])
useDict(["Niners" => 31, "Ravens" => 34.0])
useDict(["Niners" => 31.0, "Ravens" => 34.0])

// Fall back to T[] if no context is otherwise available.
var a = [1,2,3]
var a2 : Int[] = a

var b = [1,2,3.0]
var b2 : Double[] = b

var arrayOfStreams = [1...2, 3...4]

struct MyDouble { }

extension Double {
  @conversion func __conversion() -> MyDouble { return MyDouble() }
}

struct MyArray : ArrayLiteralConvertible {
  typealias Element = MyDouble

  static func convertFromArrayLiteral(elements: MyDouble...) -> MyArray {
  }
}

var myArray : MyArray = [2.5, 2.5]

// Inference for tuple elements.
var x1 = [1]
x1[0] = 0
var x2 = [(1, 2)]
x2[0] = (3, 4)
var x3 = [1, 2, 3]
x3[0] = 4

func trailingComma() {
  var a1 = [1, ]
  var a2 = [1, 2, ]
  var d1 = ["a": 1, ]
  var d2 = ["a": 1, "b": 2, ]
}

func longArray() {
  var words=["1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"]
}

