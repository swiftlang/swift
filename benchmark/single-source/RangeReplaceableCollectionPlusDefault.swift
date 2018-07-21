// RangeReplaceablePlusDefault benchmark
//
// Source: https://gist.github.com/airspeedswift/392599e7eeeb74b481a7

import TestsUtils

public var RangeReplaceableCollectionPlusDefault = BenchmarkInfo(
  name: "RangeReplaceableCollectionPlusDefault",
  runFunction: run_RangeReplaceableCollectionPlusDefault,
  tags: [.validation]
)

@inline(never)
public func run_RangeReplaceableCollectionPlusDefault(_ N: Int) {
  let stringsRef = [1, 2, 3]
  let strings = ["1", "2", "3"]
  let toInt = { (s: String) -> Int? in Int(s) }
  var a = [Int]()
  var b = [Int]()

  for _ in 1...1000*N {
    let a2: Array = mapSome(strings, toInt)
    let b2 = mapSome(strings, toInt)
    a = a2
    b = b2

    if !compareRef(a, b, stringsRef) {
      break
    }
  }

  CheckResults(compareRef(a, b, stringsRef))
}

func compareRef(_ a: [Int], _ b: [Int], _ ref: [Int]) -> Bool {
  return ref == a && ref == b
}

// This algorithm returns a generic placeholder
// that can be any kind of range-replaceable collection:
func mapSome
<S: Sequence, C: RangeReplaceableCollection>
(_ source: S, _ transform: (S.Element)->C.Element?) -> C {
  var result = C()
  for x in source {
    if let y = transform(x) {
      result.append(y)
    }
  }
  return result
}

// If you write a second version that returns an array,
// you can call the more general version for implementation:
func mapSome<S: Sequence,U>(_ source: S, _ transform: (S.Element)->U?)->[U] {
  // just calls the more generalized version
  // (works because here, the return type
  // is now of a specific type, an Array)
  return mapSome(source, transform)
}
