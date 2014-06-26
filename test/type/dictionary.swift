// RUN: %swift -parse %s -verify

// Dictionary types.
class Base {
  func f0(d: [String: Int]) { }
  func f1(d: [String: [Int: Int]]) { }
}

class Derived : Base {
  override func f0(d: Dictionary<String, Int>) { }
  override func f1(d: Dictionary<String, Dictionary<Int, Int>>) { }
}

// Dictionary types in generic specializations.
struct X<T> { }

func testGenericSpec() {
  let x = X<[Int : Int]>()
}

// Dictionary types for construction.
func constructDictionary(n: Int) {
  var dict = [Int : String](minimumCapacity: n)
  dict[5] = "hello"
}
