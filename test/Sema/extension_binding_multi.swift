// RUN: %target-swift-frontend -typecheck -primary-file %S/../Inputs/empty.swift %s -verify

struct A: Collection {
  struct Index: Comparable {}
}

extension A.Index {
  // Force validate "A".
}
