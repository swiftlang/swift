// RUN: %target-swift-frontend -parse -primary-file %S/../Inputs/empty.swift %s -verify

struct A: Collection {
  struct Index: BidirectionalIndex {}
}

extension A.Index {
  // Force validate "A".
}
