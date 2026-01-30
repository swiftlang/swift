// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library %t/test1.swift -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library %t/test2.swift -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -parse-as-library %t/test3.swift -emit-sil -o /dev/null -verify

// REQUIRES: concurrency

//--- test1.swift
@main
struct TestNonisolated1 {
  nonisolated static func main() async {} // expected-error{{main() must be '@MainActor'}}
}

//--- test2.swift
@main
struct TestNonisolated2 {
  @concurrent static func main() async {} // expected-error{{main() must be '@MainActor'}}
}

//--- test3.swift
@main
struct TestNonisolated1 {
  nonisolated(nonsending) static func main() async {} // expected-error{{main() must be '@MainActor'}}
}
