// RUN: %target-swift-frontend -parse -primary-file %S/../Inputs/empty.swift %s -verify

struct A: CollectionType {
  struct Index: BidirectionalIndexType {}
}

extension A.Index {
  // Force validate "A".
}
