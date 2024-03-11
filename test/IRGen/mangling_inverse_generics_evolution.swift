// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -o - %s -module-name test \
// RUN:   -enable-experimental-feature NoncopyableGenerics \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -parse-as-library \
// RUN:   -enable-library-evolution \
// RUN:   > %t/test.irgen

// RUN: %FileCheck %s < %t/test.irgen

public protocol P: ~Copyable { }

public protocol Hello<Person>: ~Copyable {
  // CHECK: @"$s4test5HelloP6PersonAC_AA1PTn"
  // CHECK: @"$s6Person4test5HelloPTl" =
  associatedtype Person: P & ~Copyable

  // CHECK: @"$s4test5HelloP14favoritePerson0D0QzvrTq" =
  var favoritePerson: Person { get }

  // CHECK: @"$s4test5HelloP5greetyy6PersonQzFTq"
  func greet(_ person: borrowing Person)
}

public struct Wrapper<T: ~Copyable> {
  var wrapped: T { fatalError("boom") }
}
