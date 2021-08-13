// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/errors.partial.swiftmodule -experimental-allow-module-with-compiler-errors %s
// RUN: %target-swift-frontend -merge-modules -module-name errors -emit-module -o %t/errors.swiftmodule -experimental-allow-module-with-compiler-errors %t/errors.partial.swiftmodule

public protocol SomeProto {
  init(from: SomeProto)
}

public struct A {}
public struct B: SomeProto {
  let a: A
}

public let thing = B(a: A())
