// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/errors.swiftmodule -experimental-allow-module-with-compiler-errors %s

public protocol SomeProto {
  init(from: SomeProto)
}

struct A {}
struct B: SomeProto {
  let a: A
}

let thing = B(a: A())
