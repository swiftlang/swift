// RUN: %target-swift-emit-silgen %s -module-name A | %FileCheck %s

// SILGen marks a closure [closure] when it creates its function: a closure
// expression, an autoclosure, or a declaration in a local context, such as a
// local function, a local accessor, or a `defer` body. The functions that
// reference a closure are its scopes.

@propertyWrapper struct W { var wrappedValue: Int }

func takes(_ f: () -> Int) -> Int { f() }
func takesAuto(_ x: @autoclosure () -> Int) -> Int { x() }

// CHECK-LABEL: sil hidden [ossa] @$s1A6scopesyS2iF :
func scopes(_ n: Int) -> Int {
  var counter = n
  // CHECK-LABEL: sil private [closure] [ossa] @$s1A6scopesyS2iF6$deferL_yyF :
  defer { counter += 1 }
  // CHECK-LABEL: sil private [closure] [ossa] @$s1A6scopesyS2iF5localL_SiyF :
  func local() -> Int { counter }
  // CHECK-LABEL: sil private [closure] [ossa] @$s1A6scopesyS2iF8computedL_Sivg :
  var computed: Int { counter * 2 }
  // CHECK-LABEL: sil private [closure] [ossa] @$s1A6scopesyS2iF7wrappedL_Sivg :
  // CHECK-LABEL: sil private [closure] [ossa] @$s1A6scopesyS2iF7wrappedL_Sivs :
  @W var wrapped = n
  // CHECK-LABEL: sil private [closure] [ossa] @$s1A6scopesyS2iFSiyXEfU_ :
  // CHECK-LABEL: sil private [transparent] [closure] [ossa] @$s1A6scopesyS2iFSiyXEfu_ :
  return takes { counter } + takesAuto(counter + 1) + local() + computed + wrapped
}

// A local type's members, a default argument generator, and a closure's
// foreign entry point aren't closures.
// CHECK-LABEL: sil hidden [ossa] @$s1A11notClosuresyyF :
func notClosures() {
  // CHECK-LABEL: sil private [ossa] @$s1A11notClosuresyyF9LocalTypeL_VADycfC :
  // CHECK-LABEL: sil private [ossa] @$s1A11notClosuresyyF9LocalTypeL_V6methodyyF :
  struct LocalType { func method() {} }
  LocalType().method()
  // CHECK-LABEL: sil private [ossa] @$s1A11notClosuresyyF11withDefaultL_yySiFfA_ :
  // CHECK-LABEL: sil private [closure] [ossa] @$s1A11notClosuresyyF11withDefaultL_yySiF :
  func withDefault(_ x: Int = 1) {}
  withDefault()
  // CHECK-LABEL: sil private [closure] [ossa] @$s1A11notClosuresyyFyycfU_ :
  // CHECK-LABEL: sil private [thunk] [ossa] @$s1A11notClosuresyyFyycfU_To :
  let c: @convention(c) () -> () = {}
  c()
}
