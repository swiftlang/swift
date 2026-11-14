// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %t/macros.swift -g -no-toolchain-stdlib-rpath

// NOTE: The original issue required the AST verifier to not run since otherwise
// discriminators get computed eagerly.
// RUN: %target-build-swift -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift -o %t/main -module-name main -Xfrontend -disable-ast-verifier

// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// RUN: %target-swift-frontend -emit-sil -disable-ast-verifier -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift -module-name main | %FileCheck --check-prefix CHECK-SIL %s

//--- macros.swift
import SwiftSyntax
import SwiftSyntaxMacros

public struct IdentityMacro: ExpressionMacro {
  public static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> ExprSyntax {
    "\(node.arguments)"
  }
}

//--- main.swift

@freestanding(expression)
macro id<T>(_ x: T) -> T = #externalMacro(module: "MacroDefinition", type: "IdentityMacro")

_ = #id({1}) // CHECK-SIL: sil private @$s4mainSiycfU0_ : $@convention(thin) () -> Int {
let y = {2} // CHECK-SIL: sil private @$s4mainSiycfU1_ : $@convention(thin) () -> Int {
print("top1:", y()) // CHECK: top1: 2
_ = #id({3}) // CHECK-SIL: sil private @$s4mainSiycfU3_ : $@convention(thin) () -> Int {
let z = {4} // CHECK-SIL: sil private @$s4mainSiycfU4_ : $@convention(thin) () -> Int {
print("top2:", z()) // CHECK: top2: 4

func foo() {
  let x = 0
  _ = #id({1}) // CHECK-SIL: sil private @$s4main3fooyyFSiycfU3_ : $@convention(thin) () -> Int {
  let y = {2} // CHECK-SIL: sil private @$s4main3fooyyFSiycfU0_ : $@convention(thin) () -> Int {
  print("foo1:", y()) // CHECK: foo1: 2
  _ = #id({3}) // CHECK-SIL: sil private @$s4main3fooyyFSiycfU4_ : $@convention(thin) () -> Int {
  let z = {4} // CHECK-SIL: sil private @$s4main3fooyyFSiycfU2_ : $@convention(thin) () -> Int {
  print("foo2:", z()) // CHECK: foo2: 4
}
foo()

func bar() {
  let fn = { // CHECK-SIL: sil private @$s4main3baryyFyycfU_ : $@convention(thin) () -> () {
    let x = 0
    _ = #id({1}) // CHECK-SIL: sil private @$s4main3baryyFyycfU_SiycfU3_ : $@convention(thin) () -> Int {
    let y = {2} // CHECK-SIL: sil private @$s4main3baryyFyycfU_SiycfU0_ : $@convention(thin) () -> Int {
    print("bar1:", y()) // CHECK: bar1: 2
    _ = #id({3}) // CHECK-SIL: sil private @$s4main3baryyFyycfU_SiycfU4_ : $@convention(thin) () -> Int {
    let z = {4} // CHECK-SIL: sil private @$s4main3baryyFyycfU_SiycfU2_ : $@convention(thin) () -> Int {
    print("bar2:", z()) // CHECK: bar2: 4
  }
  fn()
}
bar()

func baz() {
  let fn1 = { // CHECK-SIL: sil private @$s4main3bazyyFyycfU_ : $@convention(thin) () -> () {
    let fn2 = { // CHECK-SIL: sil private @$s4main3bazyyFyycfU_yycfU_ : $@convention(thin) () -> () {
      let x = 0
      _ = #id({1}) // CHECK-SIL: sil private @$s4main3bazyyFyycfU_yycfU_SiycfU3_ : $@convention(thin) () -> Int {
      let y = {2} // CHECK-SIL: sil private @$s4main3bazyyFyycfU_yycfU_SiycfU0_ : $@convention(thin) () -> Int {
      print("baz1:", y()) // CHECK: baz1: 2
      _ = #id({3}) // CHECK-SIL: sil private @$s4main3bazyyFyycfU_yycfU_SiycfU4_ : $@convention(thin) () -> Int {
      let z = {4} // CHECK-SIL: sil private @$s4main3bazyyFyycfU_yycfU_SiycfU2_ : $@convention(thin) () -> Int {
      print("baz2:", z()) // CHECK: baz2: 4
    }
    fn2()
  }
  fn1()
}
baz()

struct Qux {
  let fn = { // CHECK-SIL: sil private @$s4main3QuxV2fnyycvpfiyycfU_ : $@convention(thin) () -> () {
    let x = 0
    _ = #id({1}) // CHECK-SIL: sil private @$s4main3QuxV2fnyycvpfiyycfU_SiycfU3_ : $@convention(thin) () -> Int {
    let y = {2} // CHECK-SIL: sil private @$s4main3QuxV2fnyycvpfiyycfU_SiycfU0_ : $@convention(thin) () -> Int {
    print("Qux.fn1:", y()) // CHECK: Qux.fn1: 2
    _ = #id({3}) // CHECK-SIL: sil private @$s4main3QuxV2fnyycvpfiyycfU_SiycfU4_ : $@convention(thin) () -> Int {
    let z = {4} // CHECK-SIL: sil private @$s4main3QuxV2fnyycvpfiyycfU_SiycfU2_ : $@convention(thin) () -> Int {
    print("Qux.fn2:", z()) // CHECK: Qux.fn2: 4
  }
  func qux2() {
    let x = 0
    _ = #id({1}) // CHECK-SIL: sil private @$s4main3QuxV4qux2yyFSiycfU3_ : $@convention(thin) () -> Int {
    let y = {2} // CHECK-SIL: sil private @$s4main3QuxV4qux2yyFSiycfU0_ : $@convention(thin) () -> Int {
    print("Qux.qux1:", y()) // CHECK: Qux.qux1: 2
    _ = #id({3}) // CHECK-SIL: sil private @$s4main3QuxV4qux2yyFSiycfU4_ : $@convention(thin) () -> Int {
    let z = {4} // CHECK-SIL: sil private @$s4main3QuxV4qux2yyFSiycfU2_ : $@convention(thin) () -> Int {
    print("Qux.qux2:", z()) // CHECK: Qux.qux2: 4
  }
}
let qux = Qux()
qux.fn()
qux.qux2()

func flim() {
  lazy var fns = ({1}, {2}, #id({3}))
  print("flim:", fns.0(), fns.1(), fns.2()) // CHECK: flim: 1 2 3
}
flim()

func takesAutoclosure(_ x: @autoclosure () -> Int) -> Int { x() }

// Explicit closures inside an autoclosure are numbered in the enclosing
// function's sequence, but parented to the autoclosure.
func flam() {
  // CHECK-SIL: sil private @$s4main4flamyyFSiyXEfu_SiyXEfU_ : $@convention(thin) () -> Int {
  // CHECK-SIL: sil private @$s4main4flamyyFSiyXEfu_SiyXEfU1_ : $@convention(thin) () -> Int {
  print("flam:", takesAutoclosure({1}() + #id({2}()))) // CHECK: flam: 3
}
flam()

func vid() {
  struct S {
    let fn = { // CHECK-SIL: sil private @$s4main3vidyyF1SL_V2fnyycvpfiyycfU_ : $@convention(thin) () -> () {
      let x = 0
      _ = #id({1}) // CHECK-SIL: sil private @$s4main3vidyyF1SL_V2fnyycvpfiyycfU_SiycfU3_ : $@convention(thin) () -> Int {
      let y = {2} // CHECK-SIL: sil private @$s4main3vidyyF1SL_V2fnyycvpfiyycfU_SiycfU0_ : $@convention(thin) () -> Int {
      print("vid1:", y()) // CHECK: vid1: 2
      _ = #id({3}) // CHECK-SIL: sil private @$s4main3vidyyF1SL_V2fnyycvpfiyycfU_SiycfU4_ : $@convention(thin) () -> Int {
      let z = {4} // CHECK-SIL: sil private @$s4main3vidyyF1SL_V2fnyycvpfiyycfU_SiycfU2_ : $@convention(thin) () -> Int {
      print("vid2:", z()) // CHECK: vid2: 4
    }
  }
  _ = S().fn()
}
vid()

func dub() {
  lazy var fn = #id({1})
  print("dub:", fn()) // CHECK: dub: 1
}
dub()
