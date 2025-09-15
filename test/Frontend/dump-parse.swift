// RUN: not %target-swift-frontend -dump-parse %s | %FileCheck %s
// RUN: not %target-swift-frontend -dump-ast %s | %FileCheck %s -check-prefix=CHECK-AST

// CHECK-LABEL: (func_decl{{.*}}"foo(_:)"
// CHECK-AST-LABEL: (func_decl{{.*}}"foo(_:)"
func foo(_ n: Int) -> Int {
  // CHECK:   (brace_stmt
  // CHECK:     (return_stmt
  // CHECK:       (integer_literal_expr type="<null>" value="42" {{.*}})))
  // CHECK-AST: (brace_stmt
  // CHECK-AST:   (return_stmt
  // CHECK-AST:     (integer_literal_expr type="{{[^"]+}}" {{.*}} value="42" {{.*}})
    return 42
}

// -dump-parse should print an AST even though this code is invalid.
// CHECK-LABEL: (func_decl{{.*}}"bar()"
// CHECK-AST-LABEL: (func_decl{{.*}}"bar()"
func bar() {
  // CHECK: (brace_stmt
  // CHECK-NEXT:   (unresolved_decl_ref_expr type="{{[^"]+}}" name="foo"
  // CHECK-NEXT:   (unresolved_decl_ref_expr type="{{[^"]+}}" name="foo"
  // CHECK-NEXT:   (unresolved_decl_ref_expr type="{{[^"]+}}" name="foo"
  // CHECK-AST: (brace_stmt
  // CHECK-AST-NEXT:   (declref_expr type="{{[^"]+}}" {{.*}} decl="main.(file).foo@
  // CHECK-AST-NEXT:   (declref_expr type="{{[^"]+}}" {{.*}} decl="main.(file).foo@
  // CHECK-AST-NEXT:   (declref_expr type="{{[^"]+}}" {{.*}} decl="main.(file).foo@
  foo foo foo
}

// CHECK-LABEL: (enum_decl{{.*}}trailing_semi "TrailingSemi"
enum TrailingSemi {

  // CHECK-LABEL: (enum_case_decl
  // CHECK-NOT:   (enum_element_decl{{.*}}trailing_semi
  // CHECK:       (enum_element_decl{{.*}}"A")
  // CHECK:       (enum_element_decl{{.*}}"B")
  case A,B;

  // CHECK-LABEL: (subscript_decl{{.*}}trailing_semi
  // CHECK-NOT:   (func_decl{{.*}}trailing_semi <anonymous @ 0x{{[0-9a-f]+}}> get for="subscript(_:)"
  // CHECK:       (accessor_decl{{.*}} <anonymous @ 0x{{[0-9a-f]+}}> get for="subscript(_:)"
  subscript(x: Int) -> Int {
    // CHECK-LABEL: (pattern_binding_decl{{.*}}
    // CHECK-NOT:   (var_decl{{.*}}trailing_semi "y"
    // CHECK:       (var_decl{{.*}}"y"
    var y = 1;

    // CHECK-LABEL: (sequence_expr {{.*}} trailing_semi
    y += 1;

    // CHECK-LABEL: (return_stmt{{.*}}trailing_semi
    return y;
  };
};

// The substitution map for a declref should be relatively unobtrusive.
// CHECK-AST-LABEL:   (func_decl{{.*}}"generic(_:)" "<T : Hashable>" interface_type="<T where T : Hashable> (T) -> ()" access=internal captures=(<generic> {{.*}})
func generic<T: Hashable>(_: T) {}
// CHECK-AST:       (pattern_binding_decl
// CHECK-AST:         (processed_init=declref_expr type="(Int) -> ()" location={{.*}} range={{.*}} decl="main.(file).generic@{{.*}} [with (substitution_map generic_signature=<T where T : Hashable> T -> Int)]" function_ref=unapplied))
let _: (Int) -> () = generic

// Closures should be marked as escaping or not.
func escaping(_: @escaping (Int) -> Int) {}
escaping({ $0 })
// CHECK-AST:        (declref_expr type="(@escaping (Int) -> Int) -> ()"
// CHECK-AST-NEXT:        (argument_list
// CHECK-AST-NEXT:          (argument
// CHECK-AST-NEXT:            (closure_expr type="(Int) -> Int" {{.*}} discriminator=0 nonisolated escaping single_expression

func nonescaping(_: (Int) -> Int) {}
nonescaping({ $0 })
// CHECK-AST:        (declref_expr type="((Int) -> Int) -> ()"
// CHECK-AST-NEXT:        (argument_list
// CHECK-AST-NEXT:          (argument
// CHECK-AST-NEXT:            (closure_expr type="(Int) -> Int" {{.*}} discriminator=1 nonisolated single_expression

// CHECK-LABEL: (struct_decl decl_context=0x{{[0-9a-z]+}} range=[{{.+}}] "MyStruct")
struct MyStruct {}

// CHECK-LABEL: (enum_decl decl_context=0x{{[0-9a-z]+}} range=[{{.+}}] "MyEnum"
enum MyEnum {

    // CHECK-LABEL: (enum_case_decl decl_context=0x{{[0-9a-z]+}} range=[{{.+}}]
    // CHECK-NEXT:    (enum_element_decl decl_context=0x{{[0-9a-z]+}} range=[{{.+}}]  "foo(x:)"
    // CHECK-NEXT:      (parameter_list range=[{{.+}}]
    // CHECK-NEXT:         (parameter "x" decl_context=0x{{[0-9a-z]+}} apiName="x")))
    // CHECK-NEXT:     (enum_element_decl decl_context=0x{{[0-9a-z]+}} range=[{{.+}}] "bar"))
    // CHECK-NEXT:  (enum_element_decl decl_context=0x{{[0-9a-z]+}} range=[{{.+}}] "foo(x:)"
    // CHECK-NEXT:    (parameter_list range=[{{.+}}]
    // CHECK-NEXT:      (parameter "x" decl_context=0x{{[0-9a-z]+}} apiName="x")))
    // CHECK-NEXT:  (enum_element_decl decl_context=0x{{[0-9a-z]+}} range=[{{.+}}] "bar"))
    case foo(x: MyStruct), bar
}

// CHECK-LABEL: (top_level_code_decl decl_context=0x{{[0-9a-z]+}} range=[{{.+}}]
// CHECK-NEXT:    (brace_stmt implicit range=[{{.+}}]
// CHECK-NEXT:      (sequence_expr type="<null>"
// CHECK-NEXT:        (discard_assignment_expr type="<null>")
// CHECK-NEXT:        (assign_expr type="<null>"
// CHECK-NEXT:          (<null expr>)
// CHECK-NEXT:          (<null expr>))
// CHECK-NEXT:        (closure_expr type="<null>" discriminator={{[0-9]+}}
// CHECK-NEXT:          (parameter_list range=[{{.+}}]
// CHECK-NEXT:            (parameter "v" decl_context=0x{{[0-9a-z]+}}))
// CHECK-NEXT:          (brace_stmt range=[{{.+}}])))))
_ = { (v: MyEnum) in }

// CHECK-LABEL: (struct_decl decl_context=0x{{[0-9a-z]+}} range=[{{.+}}] "SelfParam"
struct SelfParam {

  // CHECK-LABEL: (func_decl decl_context=0x{{[0-9a-z]+}} range=[{{.+}}] "createOptional()" static
  // CHECK-NEXT:    (parameter "self" decl_context=0x{{[0-9a-z]+}})
  // CHECK-NEXT:    (result=type_optional
  // CHECK-NEXT:      (type_unqualified_ident id="SelfParam" unbound))
  // CHECK-NEXT:    (parameter_list range=[{{.+}}])
  static func createOptional() -> SelfParam? {

    // CHECK-LABEL: (call_expr type="<null>"
    // CHECK-NEXT:    (unresolved_decl_ref_expr type="<null>" name="SelfParam" function_ref=unapplied)
    // CHECK-NEXT:    (argument_list)
    SelfParam()
  }
}

// CHECK-LABEL: (func_decl decl_context=0x{{[0-9a-z]+}} range=[{{.+}}] "dumpMemberTypeRepr()"
// CHECK-NEXT:    (result=type_qualified_ident id="Element" unbound
// CHECK-NEXT:      (type_unqualified_ident id="Array" unbound
// CHECK-NEXT:        (type_unqualified_ident id="Bool" unbound))
// CHECK-NEXT:    (parameter_list range=[{{.+}}])
func dumpMemberTypeRepr() -> Array<Bool>.Element { true }
