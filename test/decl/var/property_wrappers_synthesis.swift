// RUN: %target-swift-frontend -typecheck -dump-ast %s | %FileCheck %s

@propertyWrapper
struct Wrapper<T> {
  var value: T

  init(initialValue: T) {
    self.value = initialValue
  }
}

protocol DefaultInit {
  init()
}

// CHECK: struct_decl{{.*}}"UseWrapper"
struct UseWrapper<T: DefaultInit> {
  // CHECK: var_decl{{.*}}"wrapped"

  // CHECK: accessor_decl{{.*}}get_for=wrapped
  // CHECK: member_ref_expr{{.*}}UseWrapper.$wrapped

  // CHECK: accessor_decl{{.*}}set_for=wrapped
  // CHECK: member_ref_expr{{.*}}UseWrapper.$wrapped

  // CHECK: accessor_decl{{.*}}_modify_for=wrapped
  // CHECK: yield_stmt
  // CHECK: member_ref_expr{{.*}}UseWrapper.wrapped
  @Wrapper
  var wrapped = T()

  // CHECK: pattern_binding_decl implicit
  // CHECK-NEXT: pattern_typed implicit type='Wrapper<T>'
  // CHECK-NEXT: pattern_named implicit type='Wrapper<T>' '$wrapped'
  // CHECK: constructor_ref_call_expr
  // CHECK-NEXT: declref_expr{{.*}}Wrapper.init(initialValue:)
  init() { }
}

struct UseWillSetDidSet {
  // CHECK: var_decl{{.*}}"z"

  // CHECK: accessor_decl{{.*}}set_for=z
  // CHECK: member_ref_expr{{.*}}UseWillSetDidSet.$z
  @Wrapper
  var z: Int {
    willSet {
      print(newValue)
    }

    didSet {
      print(oldValue)
    }
  }
}
