// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-swift-frontend -swift-version 5 -emit-sil -load-plugin-library %t/%target-library-name(MacroDefinition) %s -module-name MacroUser -o - -g | %FileCheck %s

// Verify that an expression macro expansion is described as a function inlined
// at the expansion site, both in functions and in closures.

@freestanding(expression) macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")
@freestanding(expression) macro multiStatement() -> Int = #externalMacro(module: "MacroDefinition", type: "MultiStatementClosure")

func use<T>(_ t: T) {}

func inFunction(a: Int) {
  use(#stringify(a))
}
// CHECK: sil_scope [[FN:[0-9]+]] { loc "{{.*}}macro_expand_in_closure_debuginfo.swift":[[@LINE-3]]:6 parent @$s9MacroUser10inFunction1aySi_tF :
// CHECK: sil_scope [[FN_SITE:[0-9]+]] { loc "{{.*}}macro_expand_in_closure_debuginfo.swift":[[@LINE-3]]:7 parent [[FN]] }
// CHECK: sil_scope [[FN_MACRO:[0-9]+]] { loc "@__swiftmacro_{{.*}}9stringifyfMf_.swift":1:1 parent @$s9MacroUser{{.*}}9stringifyfMf_ : {{.*}} inlined_at [[FN_SITE]] }
// CHECK: sil hidden @$s9MacroUser10inFunction1aySi_tF :
// CHECK: string_literal utf8 "a", loc "@__swiftmacro_{{.*}}9stringifyfMf_.swift":1:5, scope [[FN_MACRO]]

func inClosure(a: Int) {
  let body = {
    use(#stringify(a))
  }
  body()
}
// CHECK: sil_scope [[CL:[0-9]+]] { loc "{{.*}}macro_expand_in_closure_debuginfo.swift":[[@LINE-5]]:14 parent @$s9MacroUser9inClosure1aySi_tFyycfU_ :
// CHECK: sil_scope [[CL_SITE:[0-9]+]] { loc "{{.*}}macro_expand_in_closure_debuginfo.swift":[[@LINE-5]]:9 parent [[CL]] }
// CHECK: sil_scope [[CL_MACRO:[0-9]+]] { loc "@__swiftmacro_{{.*}}9stringifyfMf_.swift":1:1 parent @$s9MacroUser{{.*}}9stringifyfMf_ : {{.*}} inlined_at [[CL_SITE]] }
// CHECK: sil private @$s9MacroUser9inClosure1aySi_tFyycfU_ :
// CHECK: string_literal utf8 "a", loc "@__swiftmacro_{{.*}}9stringifyfMf_.swift":1:5, scope [[CL_MACRO]]

// A closure that is part of the expansion is not inlined anywhere: its body
// points directly into the macro buffer.
func multiStatementInClosure() {
  let body = {
    use(#multiStatement())
  }
  body()
}
// CHECK: sil_scope [[MS:[0-9]+]] { loc "{{.*}}macro_expand_in_closure_debuginfo.swift":[[@LINE-5]]:14 parent @$s9MacroUser23multiStatementInClosureyyFyycfU_ :
// CHECK: sil_scope [[MS_SITE:[0-9]+]] { loc "{{.*}}macro_expand_in_closure_debuginfo.swift":[[@LINE-5]]:9 parent [[MS]] }
// CHECK: sil_scope [[MS_MACRO:[0-9]+]] { loc "@__swiftmacro_{{.*}}14multiStatementfMf_.swift":1:1 parent @$s9MacroUser{{.*}}14multiStatementfMf_ : {{.*}} inlined_at [[MS_SITE]] }
// CHECK: sil private @$s9MacroUser23multiStatementInClosureyyFyycfU_ :
// CHECK: apply {{.*}}, loc "@__swiftmacro_{{.*}}14multiStatementfMf_.swift":1:1, scope [[MS_MACRO]]
// CHECK: sil_scope {{[0-9]+}} { loc "@__swiftmacro_{{.*}}14multiStatementfMf_.swift":1:1 parent @$s9MacroUser23multiStatementInClosureyyFyycfU_SiyXEfU_ : $@convention(thin) () -> Int }{{$}}
// CHECK: sil private @$s9MacroUser23multiStatementInClosureyyFyycfU_SiyXEfU_ :
// CHECK: integer_literal $Builtin.Int{{64|32}}, 10, loc "@__swiftmacro_{{.*}}14multiStatementfMf_.swift":2:14, scope {{[0-9]+}}
