// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Building this macro takes some time, so amortise the cost by using it for multiple sub tests
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(UnstringifyMacroDefinition) -module-name=UnstringifyMacroDefinition \
// RUN:   %S/Inputs/unstringify-macro.swift -g -no-toolchain-stdlib-rpath



// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/single.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/single.swift
// RUN: %diff %t/single.swift %t/single.swift.expected

// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/multiple.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/multiple.swift
// RUN: %diff %t/multiple.swift %t/multiple.swift.expected

// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/existing.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/existing.swift
// RUN: %diff %t/existing.swift %t/existing.swift.expected

// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/gone.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/gone.swift
// RUN: %diff %t/gone.swift %t/gone.swift.expected

// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/wrong-location.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/wrong-location.swift
// RUN: %diff %t/wrong-location.swift %t/wrong-location.swift.expected

// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/nested.swift 2>%t/output.txt
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/nested.swift
// RUN: %diff %t/nested.swift %t/nested.swift.expected

// RUN: not %target-swift-frontend-verify -I %t -plugin-path %swift-plugin-dir -typecheck %t/unparsed.swift 2>%t/output.txt -Rmacro-expansions
// RUN: not %update-verify-tests < %t/output.txt | %FileCheck --check-prefix CHECK-UNPARSED %s

// RUN: not %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/escaped.swift 2>%t/output.txt -Rmacro-expansions
// RUN: %update-verify-tests < %t/output.txt
// RUN: %target-swift-frontend-verify -load-plugin-library %t/%target-library-name(UnstringifyMacroDefinition) -typecheck %t/escaped.swift -Rmacro-expansions
// RUN: %diff %t/escaped.swift %t/escaped.swift.expected

//--- single.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("""
func foo(_ x: Int) {
    let a = x
}
""")
func foo() {}

//--- single.swift.expected
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1{{in expansion of macro 'unstringifyPeer' on global function 'foo()' here}}
@unstringifyPeer("""
func foo(_ x: Int) {
    let a = x
}
""")
// expected-expansion@+3:14{{
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
// }}
func foo() {}

//--- multiple.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("""
func foo(_ x: Int) {
    a = 2
    b = x
}
""")
func foo() {}

//--- multiple.swift.expected
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1 4{{in expansion of macro 'unstringifyPeer' on global function 'foo()' here}}
@unstringifyPeer("""
func foo(_ x: Int) {
    a = 2
    b = x
}
""")
// expected-expansion@+5:14{{
//   expected-note@1 2{{'x' declared here}}
//   expected-error@2{{cannot find 'a' in scope; did you mean 'x'?}}
//   expected-error@3{{cannot find 'b' in scope; did you mean 'x'?}}
// }}
func foo() {}

//--- existing.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("""
func foo(_ x: Int) {
    a = 2
    b = x
}
""")
  //expected-expansion@+4:14{{
    //   expected-note@1   {{'x' declared here}}
    //   expected-error@3  {{cannot find 'b' in scope; did you mean 'x'?}}
  //}}
func foo() {}

//--- existing.swift.expected
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1 4{{in expansion of macro 'unstringifyPeer' on global function 'foo()' here}}
@unstringifyPeer("""
func foo(_ x: Int) {
    a = 2
    b = x
}
""")
  //expected-expansion@+5:14{{
    //   expected-note@1   2{{'x' declared here}}
    //   expected-error@2   {{cannot find 'a' in scope; did you mean 'x'?}}
    //   expected-error@3  {{cannot find 'b' in scope; did you mean 'x'?}}
  //}}
func foo() {}

//--- gone.swift
  //expected-expansion@+4:14{{
    //   expected-note@1   {{'x' declared here}}
    //   expected-error@3  {{cannot find 'b' in scope; did you mean 'x'?}}
  //}}
func foo() {}

//--- gone.swift.expected
func foo() {}

//--- wrong-location.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

  // expected-expansion@2:14{{
    //   expected-note@2 {{'x' declared here}}
    //   expected-error@3 {{cannot find 'b' in scope; did you mean 'x'?}}
  // }}
@unstringifyPeer("""
func foo(_ x: Int) {
    a = 2
    b = x
}
""")
func foo() {}

//--- wrong-location.swift.expected
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1 4{{in expansion of macro 'unstringifyPeer' on global function 'foo()' here}}
@unstringifyPeer("""
func foo(_ x: Int) {
    a = 2
    b = x
}
""")
// expected-expansion@+5:14{{
//   expected-note@1 2{{'x' declared here}}
//   expected-error@2{{cannot find 'a' in scope; did you mean 'x'?}}
//   expected-error@3{{cannot find 'b' in scope; did you mean 'x'?}}
// }}
func foo() {}

//--- nested.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
// hack to make this seem non-recursive
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("""
func bar(_ y: Int) {
    @unstringifyPeer2(\"""
    func foo(_ x: Int) {
        a = 2
        b = x
    }
    \""")
    func foo() {}
    foo(y)
}
""")
func bar() {}

//--- nested.swift.expected
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")
// hack to make this seem non-recursive
@attached(peer, names: overloaded)
macro unstringifyPeer2(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1 7{{in expansion of macro 'unstringifyPeer' on global function 'bar()' here}}
@unstringifyPeer("""
func bar(_ y: Int) {
    @unstringifyPeer2(\"""
    func foo(_ x: Int) {
        a = 2
        b = x
    }
    \""")
    func foo() {}
    foo(y)
}
""")
// expected-expansion@+10:14{{
//   expected-note@1 2{{did you mean 'y'?}}
//   expected-note@2 4{{in expansion of macro 'unstringifyPeer2' on local function 'foo()' here}}
//   expected-expansion@9:6{{
//     expected-note@1 2{{did you mean 'x'?}}
//     expected-error@2{{cannot find 'a' in scope}}
//     expected-error@3{{cannot find 'b' in scope}}
//   }}
//   expected-error@10{{argument passed to call that takes no arguments}}
// }}
func bar() {}

//--- unparsed.h
// CHECK-UNPARSED: no files updated: found diagnostics in unparsed files TMP_DIR{{/|\\}}unparsed.h
void foo(int len, int *p) __attribute__((swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(2), count: \"len\"))")));

//--- module.modulemap
module UnparsedClang {
  header "unparsed.h"
  export *
}

//--- unparsed.swift
import UnparsedClang

func bar() {
  let a: CInt = 1
  var b: CInt = 13
  foo(a, &b)
}

//--- escaped.swift
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

@unstringifyPeer("""
func foo(_ x: Int) {
    let a = "\\(x)"
    let b = "\\(x)"
}
""")
// NB: DiagnosticVerifier interprets "\\(x)" as "\(x)"
// expected-expansion@+3:30{{
//   expected-remark@2{{macro content: |let a = "\\(x)"|}}
// }}
func foo() { let _ = "\(2)" }

//--- escaped.swift.expected
@attached(peer, names: overloaded)
macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "UnstringifyMacroDefinition", type: "UnstringifyPeerMacro")

// expected-note@+1 6{{in expansion of macro 'unstringifyPeer' on global function 'foo()' here}}
@unstringifyPeer("""
func foo(_ x: Int) {
    let a = "\\(x)"
    let b = "\\(x)"
}
""")
// NB: DiagnosticVerifier interprets "\\(x)" as "\(x)"
// expected-expansion@+8:30{{
//   expected-remark@1{{macro content: |func foo(_ x: Int) {|}}
//   expected-warning@2{{initialization of immutable value 'a' was never used; consider replacing with assignment to '_' or removing it}}
//   expected-remark@2{{macro content: |    let a = "\\(x)"|}}
//   expected-warning@3{{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
//   expected-remark@3{{macro content: |    let b = "\\(x)"|}}
//   expected-remark@4{{macro content: |}|}}
// }}
func foo() { let _ = "\(2)" }

