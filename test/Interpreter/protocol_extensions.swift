// RUN: %target-run-simple-swift | FileCheck %s

extension SequenceType {
  var myCount: Int {
    var result = 0
    for x in self {
      ++result
    }
    return result
  }
}

// CHECK: 4
println(["a", "b", "c", "d"].myCount)

extension CollectionType {
  var myIndices: Range<Index> {
    return Range(start: startIndex, end: endIndex)
  }
}

extension CollectionType {
  func indexMatching(fn: Generator.Element -> Bool) -> Index? {
    for i in myIndices {
      if fn(self[i]) { return i }
    }
    return nil
  }
}

// CHECK: 2
println(["a", "b", "c", "d"].indexMatching({$0 == "c"})!)

extension CollectionType where Self.Generator.Element : Equatable {
  func myIndexOf(element: Generator.Element) -> Index? {
    for i in indices(self) {
      if self[i] == element { return i }
    }

    return nil
  }
}

// CHECK: 3
println(["a", "b", "c", "d", "e"].myIndexOf("d")!)

// CHECK: DONE
println("DONE")
