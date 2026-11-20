// rdar://180918530
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -primary-file %t/use.swift -primary-file %t/conformance.swift

//--- use.swift
func use() {
  let _: S.EntryTuple? = nil
}

//--- conformance.swift
struct Region {
  var start: Int
}

protocol P {
  associatedtype Entry
  typealias EntryTuple = (region: Region, entry: Entry)

  func entry() -> Entry
  func tuples() -> [EntryTuple]
}

struct S: P {
  func entry() -> Int { 0 }

  func tuples() -> [S.EntryTuple] {
    let tuples: [(region: Region, entry: Int)] = []
    return tuples.sorted { $0.region.start < $1.region.start } // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
  }
}
