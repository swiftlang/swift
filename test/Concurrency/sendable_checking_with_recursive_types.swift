// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -verify -language-mode 6 -emit-sil -c %t/use.swift -primary-file %t/types.swift
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -verify -language-mode 6 -emit-sil -c %t/types.swift -primary-file %t/use.swift

// REQUIRES: concurrency

//--- types.swift

// https://github.com/swiftlang/swift/issues/62060
// Test1 references Test2 that references itself
enum Test1 {
  case a(i: Int, b: Test2)
}
indirect enum Test2 {
  case b(i: Int, b: Test2)
  case c
}

// https://github.com/swiftlang/swift/issues/82628
// Multi-step recursion
enum Test3 {
  case v1(data: S1)
  case v2
  case v3
}

struct S1 {
  let s2: [S2]
}

struct S2 {
  let t4: Test4
}

enum Test4 {
  case v1(a: S1)
  case v2
  case v3
}

// Recrusion through a typealias (Sendable check here is triggered by SIL at use)
enum RecWithAliases {
    typealias RecursiveArray = [RecWithAliases]
    typealias RecursiveDictionary = [(String, RecWithAliases)]

    case array(RecursiveArray)
    case entity(RecursiveDictionary)
}

//--- use.swift
func testAlias() -> RecWithAliases {
  let v = RecWithAliases.RecursiveDictionary()
  return .entity(v)
}
