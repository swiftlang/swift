// RUN: %target-swift-frontend -dump-ast %s | %FileCheck %s

protocol P {
  associatedtype A

  @Builder<A>
  var x1: [S] { get }
    
  @Builder<Self>
  var x2: [S] { get }
}

@resultBuilder
enum Builder<T> {
  static func buildBlock(_ args: S...) -> [S] { args }
}

struct S {}

// CHECK: struct_decl{{.*}}TestGenericBuilderInference
struct TestGenericBuilderInference: P {
  typealias A = Int

  // CHECK: var_decl{{.*}}x1
  // CHECK: Builder.buildBlock{{.*}}(substitution_map generic_signature=<T> (substitution T -> Int))
  var x1: [S] { S() }

  // CHECK: var_decl{{.*}}x2
  // CHECK: Builder.buildBlock{{.*}}(substitution_map generic_signature=<T> (substitution T -> TestGenericBuilderInference))
  var x2: [S] { S() }
}
