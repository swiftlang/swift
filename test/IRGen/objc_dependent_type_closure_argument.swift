// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -primary-file %s -enable-objc-interop -import-objc-header %S/Inputs/objc_dependent_type_closure_argument.h
// REQUIRES: objc_codegen

// Regression test for https://github.com/apple/swift/pull/40295
public protocol SwiftProtocol {
  associatedtype T: AnyObject
}

public class SwiftClass<S: SwiftProtocol> {
  static func foo(objcClass: ObjCClass<S.T>) {
    objcClass.bar(block: { _ in })
  }
}
