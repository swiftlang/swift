// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(UnstringifyMacroDefinition) -module-name=UnstringifyMacroDefinition %S/Inputs/macro/unstringify-macro.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend-verify -swift-version 5 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -Rmacro-expansions -typecheck %t/main.swift
// RUN: not %target-swift-frontend-verify -swift-version 5 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -Rmacro-expansions -typecheck %t/empty-no-expansion.swift 2>&1 | %FileCheck --check-prefix=CHECK-MISSING %t/empty-no-expansion.swift

// RUN: %target-swift-frontend-verify -swift-version 5 -verify-child-notes -typo-correction-limit 10 -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/cross-buffer-location-positive.swift
// RUN: not %target-swift-frontend-verify -swift-version 5 -verify-child-notes -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/cross-buffer-location-negative.swift 2>&1 | %FileCheck %t/cross-buffer-location-negative.swift --implicit-check-not error: --implicit-check-not warning:

//--- main.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1 *{{in expansion of macro 'unstringifyPeer' on global function 'foo()' here}}
@unstringifyPeer("func foo(_ x: Int) {\nlet a = 2\nlet b = x\n}")
func foo() {}
/*
expected-expansion@-2:14{{
    expected-remark@1{{macro content: |func foo(_ x: Int) {|}}
    expected-remark@2{{macro content: |    let a = 2|}}
    expected-ignored-error@42{{this diagnostic should not affect the expansion}}
    expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
    expected-remark@3{{macro content: |    let b = x|}}
    expected-warning@3{{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
    expected-remark@4{{macro content: |}|}}
}}
*/

@freestanding(expression) public macro ExprMacro() -> String = #file

func bar() {
    // expected-note@+1{{in expansion of macro 'ExprMacro' here}}
    let _ = #ExprMacro()
    /*
    expected-expansion@-2:13{{
        expected-remark@1{{macro content: |#file|}}
    }}
    */
}

//--- empty-no-expansion.swift
// CHECK-MISSING:     error: expected expansion not produced
// CHECK-MISSING-NOT: didn't find '}}'
// CHECK-MISSING-NOT: no expansion with diagnostics starting at

// expected-expansion@+2:14{{
// }}
func qux() {}

//--- cross-buffer-location-positive.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("func foo(_ x: Int) {\na = 2\nb = x\n}") // #here
func foo() {}
// Every diagnostic emitted inside the expansion carries the same "in expansion
// of macro" child note, whose location is the '@unstringifyPeer' attribute in
// this (outer) file.
// expected-expansion@-4:14{{
//   expected-note@1 2{{'x' declared here}} {{children:
//     expected-note@#here{{in expansion of macro 'unstringifyPeer' on global function 'foo()' here}}
//   }}
//   expected-error@2{{cannot find 'a' in scope; did you mean 'x'?}} {{children:
//     expected-note@#here{{in expansion of macro 'unstringifyPeer' on global function 'foo()' here}}
//   }}
//   expected-error@3{{cannot find 'b' in scope; did you mean 'x'?}} {{children:
//     expected-note@#here{{in expansion of macro 'unstringifyPeer' on global function 'foo()' here}}
//   }}
// }}

//--- cross-buffer-location-negative.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// The child note message is wrong, so the real child note is reported as
// unexpected and the expected child note is not produced.
// CHECK: :[[@LINE+1]]:1: error: unexpected child note produced: in expansion of macro 'unstringifyPeer' on global function 'foo()' here
@unstringifyPeer("func foo(_ y: Int) {\nqqq()\n}") // #here2
func foo() {}
// expected-expansion@-1:14{{
//   CHECK: :[[@LINE+1]]:6: note: for parent matched here
//   expected-error@2{{cannot find 'qqq' in scope}} {{children:
//     CHECK: :[[@LINE+1]]:8: error: expected note not produced
//     expected-note@#here2{{WRONG MESSAGE}}
//   }}
// }}
