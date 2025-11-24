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
    //   expected-error@3  {{cannot find 'b' in scope; did you mean 'x'?}}
    //   expected-note@1   2{{'x' declared here}}
    //   expected-error@2   {{cannot find 'a' in scope; did you mean 'x'?}}
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

