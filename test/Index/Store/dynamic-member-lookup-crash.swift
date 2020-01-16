// Ensure that we don't crash looking for default implementations during
// indexing.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -index-store-path %t/idx -o %t/file.o -typecheck -primary-file %s -verify

@dynamicMemberLookup
protocol B {
  associatedtype Value

  var value: Value { get }
  
  subscript<Subject>(dynamicMember dynamicMember: WritableKeyPath<Value, Subject>) -> Value { get }
}

extension B {
    subscript<Subject>(dynamicMember dynamicMember: WritableKeyPath<Value, Subject>) -> Value {
    get { value }
  }
}

struct Foo<Value>: B {
  var value: Value  
}
