// Ensure that we don't crash looking for default implementations during indexing.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -index-store-path %t/idx -o %t/file.o -typecheck -primary-file %s -verify

@dynamicMemberLookup
public protocol Foo {
  associatedtype Value
  
  var value: Value { get }
  subscript<U>(dynamicMember keyPath: KeyPath<Value, U>) -> U { get }
}

extension Foo {
  public var value: Value {
    fatalError()
  }
}
