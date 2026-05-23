// RUN: %target-typecheck-verify-swift  -enable-experimental-feature SuppressedAssociatedTypesWithDefaults

// REQUIRES: swift_feature_SuppressedAssociatedTypesWithDefaults

protocol Recur1<Assoc>: ~Copyable {
    associatedtype Assoc: Recur1, ~Copyable where Assoc.Assoc: ~Copyable
                                               // ^~~~~~~~~~~~~~~~~~~~~~
                                               // The important difference.
}

struct TestRecur1: Recur1, ~Copyable {
  typealias Assoc = TestRecur1
}

struct TestRecur1_Expanded: Recur1, ~Copyable {
  struct Assoc: Recur1, ~Copyable {
    struct Assoc: Recur1, ~Copyable {
      struct Assoc: Recur1, ~Copyable {
        struct Assoc: Recur1, ~Copyable {
          typealias Assoc = TestRecur1_Expanded
        }
      }
    }
  }
}

protocol Recur2<Assoc>: ~Copyable {
    associatedtype Assoc: Recur2, ~Copyable
}

struct TestRecur2: Recur2, ~Copyable {  // expected-error {{'TestRecur2' does not conform to protocol 'Recur2'}}
                                        // expected-error@-1 {{type 'TestRecur2.Assoc' (aka 'TestRecur2') does not conform to protocol 'Copyable'}}

  typealias Assoc = TestRecur2
}
