// REQUIRES: swift_swift_parser, objc_interop
// REQUIRES: swift_feature_MacrosOnImports

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -parse-as-library \
// RUN:    -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -typecheck %t/test.swift -verify -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -import-bridging-header %t%{fs-sep}test.h \
// RUN:    -verify-additional-file %t%{fs-sep}test.h -enable-experimental-feature MacrosOnImports -module-name Main -Rmacro-expansions

// This exercises the unqualified lookup for finding decls in macro expansions,
// and serves as a regression test to make sure the name lookup checks
// auxiliary decls of top level decls whose base name matches even when the
// compound name does not, since the compound name of the auxiliary decl might.

//--- test.h
#import <Foundation/Foundation.h>

void cFunctionInHeader(int x);

@interface A : NSObject
// expected-expansion@+8:20{{
//   expected-remark@1{{macro content: |@objc @implementation public func jkl(_ x: CInt) {|}}
//   expected-remark@2{{macro content: |    print(x)|}}
//   expected-remark@3{{macro content: |}|}}
// }}
// expected-expansion@+3:1{{
//   expected-note@1 3{{in expansion of macro 'unstringifyPeer' on instance method 'jkl' here}}
// }}
- (void)jkl: (int)x __attribute__((swift_attr("@Main.unstringifyPeer(\"@objc @implementation public func jkl(_ x: CInt) { print(x) }\")")));
@end

@interface B : NSObject
- (void)methodFromHeader1:(int)param;
@end

//--- test.swift
import Foundation

@attached(peer, names: overloaded)
public macro unstringifyPeer(_ s: String) =
    #externalMacro(module: "MacroDefinition", type: "UnstringifyPeerMacro")



// expected-note@+1 3{{in expansion of macro 'unstringifyPeer' on global function 'cFunctionInHeader()' here}}
@unstringifyPeer("""
@c @implementation
public func cFunctionInHeader(_ x: CInt) {}
""")
// expected-expansion@+5:35{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func cFunctionInHeader(_ x: CInt) {|}}
//   expected-remark@3{{macro content: |}|}}
// }}
public func cFunctionInHeader() {}

public func qwerty(_ x: A) {
  x.jkl(1)
}

// FIXME: Expand macros before declaring extension inadiquate.

// expected-note@+3{{add stub for missing '@implementation' requirement}}
// expected-note@+2{{missing instance method 'method(fromHeader1:)'}}
// expected-error@+1{{extension for main class interface does not provide all required implementations}}
@objc @implementation extension B {
  // expected-note@+1 4{{in expansion of macro 'unstringifyPeer' on instance method 'method()' here}}
  @unstringifyPeer("""
  @objc @implementation public func method(fromHeader1: CInt) {
    print(fromHeader1)
  }
  """)
  public final func method() {
  // expected-expansion@+6:4{{
  //   expected-error@1{{could not find imported function '' matching instance method 'method(fromHeader1:)'; make sure you import the module or header that declares it}}
  //   expected-remark@1{{macro content: |@objc @implementation public func method(fromHeader1: CInt) {|}}
  //   expected-remark@2{{macro content: |  print(fromHeader1)|}}
  //   expected-remark@3{{macro content: |}|}}
  // }}
  }
}
