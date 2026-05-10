// RUN: %target-swift-frontend -target %target-cpu-apple-macosx10.15 -typecheck -dump-ast %s | %FileCheck %s

// REQUIRES: OS=macosx

class AnyP: P {
  init<T: P>(erasing: T) {}
}

@_typeEraser(AnyP)
@_typeEraser(NewAnyP)
protocol P {}

struct ConcreteP: P, Hashable {}

// CHECK-LABEL: testBasic
dynamic func testBasic() -> some P {
  // CHECK:      underlying_to_opaque_expr{{.*}}"some P"
  // CHECK-NEXT:   call_expr implicit type="AnyP"
  // CHECK:          argument_list implicit labels="erasing:"
  // CHECK-NEXT:       argument label="erasing"
  // CHECK-NEXT:         call_expr type="ConcreteP"
  ConcreteP()
}

// CHECK-LABEL: testTypeAlias
typealias AliasForP = P
dynamic func testTypeAlias() -> some AliasForP {
  // CHECK:      underlying_to_opaque_expr{{.*}}"some P"
  // CHECK-NEXT:   call_expr implicit type="AnyP"
  // CHECK:          argument_list implicit labels="erasing:"
  // CHECK-NEXT:       argument label="erasing"
  // CHECK-NEXT:         call_expr type="ConcreteP"
  ConcreteP()
}

// CHECK-LABEL: testNoDynamic
func testNoDynamic() -> some P {
  // CHECK: underlying_to_opaque_expr{{.*}}"some P"
  // CHECK-NEXT: call_expr type="ConcreteP"
  ConcreteP()
}

// CHECK-LABEL: testNoOpaque
dynamic func testNoOpaque() -> P {
  // CHECK: erasure_expr implicit type="any P"
  // CHECK-NEXT: normal_conformance type="ConcreteP" protocol="P"
  // CHECK: call_expr type="ConcreteP"
  ConcreteP()
}

// CHECK-LABEL: testComposition
typealias Composition = P & Hashable
dynamic func testComposition() -> some Composition {
  // CHECK: underlying_to_opaque_expr{{.*}}"some Hashable & P"
  // CHECK-NEXT: call_expr type="ConcreteP"
  ConcreteP()
}

// CHECK-LABEL: struct_decl{{.*}}Builder
@resultBuilder
struct Builder {
  static func buildBlock(_ params: P...) -> ConcreteP {
    return ConcreteP()
  }
}

// CHECK-LABEL: TestResultBuilder
class TestResultBuilder {
  // CHECK-LABEL: testTransformFnBody
  @Builder dynamic var testTransformFnBody: some P {
    // CHECK:      return_stmt
    // CHECK-NEXT:   underlying_to_opaque_expr implicit type="some P"
    // CHECK-NEXT:     call_expr implicit type="AnyP"
    // CHECK:            argument_list implicit labels="erasing:"
    // CHECK-NEXT:         argument label="erasing"
    // CHECK:                declref_expr implicit type="@lvalue ConcreteP"
    ConcreteP()
  }

  // CHECK-LABEL: func_decl{{.*}}takesBuilder
  func takesBuilder(@Builder closure: () -> ConcreteP) -> ConcreteP { closure() }

  // CHECK-LABEL: testClosureBuilder
  dynamic var testClosureBuilder: some P {
    // CHECK:      underlying_to_opaque_expr implicit type="some P"
    // CHECK-NEXT:   call_expr implicit type="AnyP"
    // CHECK:          argument_list implicit labels="erasing:"
    // CHECK-NEXT:       argument label="erasing"
    // CHECK:              closure_expr type="() -> ConcreteP"
    takesBuilder {
      // CHECK: return_stmt
      // CHECK-NEXT: call_expr implicit type="ConcreteP"
      ConcreteP()
    }
  }
}

// CHECK-LABEL: class_decl{{.*}}DynamicReplacement
class DynamicReplacement {
  dynamic func testDynamicReplaceable() -> some P {
    // CHECK:      underlying_to_opaque_expr implicit type="some P"
    // CHECK-NEXT:   call_expr implicit type="AnyP"
    // CHECK:          argument_list implicit labels="erasing:"
    // CHECK-NEXT:       argument label="erasing"
    // CHECK-NEXT:         call_expr type="ConcreteP"
    ConcreteP()
  }
}

// CHECK-LABEL: extension_decl{{.*}}DynamicReplacement
extension DynamicReplacement {
  // CHECK-LABEL: testDynamicReplacement
  @_dynamicReplacement(for: testDynamicReplaceable)
  func testDynamicReplacement() -> some P {
    print("not single expr return")
    // CHECK:      return_stmt
    // CHECK-NEXT:   underlying_to_opaque_expr implicit type="some P"
    // CHECK-NEXT:     call_expr implicit type="AnyP"
    // CHECK:            argument_list implicit labels="erasing:"
    // CHECK-NEXT:         argument label="erasing"
    // CHECK-NEXT:           call_expr type="ConcreteP"
    return ConcreteP()
  }
}

@available(macOS 100, *)
struct NewAnyP: P {
  init(erasing: some P) {}
}

@available(macOS 100, *)
struct NewConcreteP: P {}

// CHECK-LABEL: var_decl{{.*}}x0
dynamic var x0: some P {
  // CHECK:      return_stmt
  // CHECK-NEXT:   underlying_to_opaque_expr implicit type="some P"
  // CHECK-NEXT:     call_expr implicit type="AnyP"
  ConcreteP()
}

// CHECK-LABEL: var_decl{{.*}}x1
@available(macOS 99, *)
dynamic var x1: some P {
  // CHECK:      return_stmt
  // CHECK-NEXT:   underlying_to_opaque_expr implicit type="some P"
  // CHECK-NEXT:     call_expr implicit type="AnyP"
  ConcreteP()
}

// CHECK-LABEL: var_decl{{.*}}x2
@available(macOS 100, *)
dynamic var x2: some P {
  // CHECK:      return_stmt
  // CHECK-NEXT:   underlying_to_opaque_expr implicit type="some P"
  // CHECK-NEXT:     call_expr implicit type="NewAnyP"
  ConcreteP()
}

// CHECK-LABEL: var_decl{{.*}}x3
@available(macOS 101, *)
dynamic var x3: some P {
  // CHECK:      return_stmt
  // CHECK-NEXT:   underlying_to_opaque_expr implicit type="some P"
  // CHECK-NEXT:     call_expr implicit type="NewAnyP"
  ConcreteP()
}

// CHECK-LABEL: var_decl{{.*}}x4
dynamic var x4: some P {
  if #available(macOS 101, *) {
    // CHECK:      return_stmt
    // CHECK-NEXT:   underlying_to_opaque_expr implicit type="some P"
    // CHECK-NEXT:     call_expr implicit type="NewAnyP"
    return NewConcreteP()
  } else {
    // CHECK:      return_stmt
    // CHECK-NEXT:   underlying_to_opaque_expr implicit type="some P"
    // CHECK-NEXT:     call_expr implicit type="AnyP"
    return ConcreteP()
  }
}
