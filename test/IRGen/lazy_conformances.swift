// RUN: %target-swift-frontend -parse-as-library -O %s -emit-ir | %FileCheck %s

// CHECK: @"$s17lazy_conformances12MyCollectionVyxGSKAASKRzrlMc" = hidden constant {
// CHECK: @"$s17lazy_conformances12MyCollectionVyxGSTAAMc" = hidden constant {

struct MyCollection<Base : Collection> : Collection {
  typealias Index = Base.Index
  typealias Element = Base.Element

  var startIndex: Index {
    fatalError()
  }

  var endIndex: Index {
    fatalError()
  }

  func index(after i: Index) -> Index {
    fatalError()
  }

  func formIndex(after i: inout Index) {
    fatalError()
  }

  subscript(position: Index) -> Element {
    fatalError()
  }
}

extension MyCollection : BidirectionalCollection
  where Base : BidirectionalCollection
{
  func index(before i: Index) -> Index {
    fatalError()
  }

  func formIndex(before i: inout Index) {
    fatalError()
  }
}
