// RUN: %target-swift-frontend -typecheck -verify %s

struct A<T> {
  // expected-note @+1 {{candidate has non-matching type}}
  func enqueue(operation: @escaping @Sendable () async -> T) {}
}

protocol AnnotatedEnqueuer {
  associatedtype Result

  // expected-note @+1 {{protocol requires function}}
  func enqueue(operation: @escaping @isolated(any) @Sendable () async -> Result)
}

// expected-error@+2 {{type 'A<T>' does not conform to protocol 'AnnotatedEnqueuer'}}
// expected-note@+1 {{add stubs for conformance}}
extension A : AnnotatedEnqueuer {}
