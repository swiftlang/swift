// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -module-name NamespaceLib -enable-experimental-feature Namespaces %t/library.swift -o %t/NamespaceLib.swiftmodule
// RUN: %llvm-bcanalyzer -dump %t/NamespaceLib.swiftmodule | %FileCheck %s --check-prefix=MODULE
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -I %t -enable-experimental-feature Namespaces %t/client.swift
// RUN: %target-swift-frontend -emit-module -module-name NamespaceWrapper -I %t -enable-experimental-feature Namespaces %t/wrapper.swift -o %t/NamespaceWrapper.swiftmodule
// RUN: %target-swift-frontend -emit-ir -O -I %t -enable-experimental-feature Namespaces %t/use.swift -o /dev/null
// RUN: %target-swift-frontend -emit-module -module-name NamespaceLib -enable-experimental-feature Namespaces -enable-experimental-feature ParserASTGen %t/library.swift -o %t/NamespaceLib.swiftmodule
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -I %t -enable-experimental-feature Namespaces -enable-experimental-feature ParserASTGen %t/client.swift
// RUN: %target-swift-frontend -emit-module -module-name NamespaceWrapper -I %t -enable-experimental-feature Namespaces -enable-experimental-feature ParserASTGen %t/wrapper.swift -o %t/NamespaceWrapper.swiftmodule
// RUN: %target-swift-frontend -emit-ir -O -I %t -enable-experimental-feature Namespaces -enable-experimental-feature ParserASTGen %t/use.swift -o /dev/null

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_Namespaces

// MODULE: <NAMESPACE_DECL

// The inaccessible-member diagnostic includes a note in a synthesized source
// file for the imported declaration; verify the client diagnostics here.

//--- library.swift
namespace BuildProof {
  @inline(never) public static func answer() -> Int { 42 }
  public static func answer(_ value: Int) -> Int { value }
  static func hidden() -> Int { 0 }
}

//--- client.swift
import NamespaceLib

let answer: () -> Int = BuildProof.answer
let withArgument: (Int) -> Int = BuildProof.answer
let cFunction: @convention(c) () -> Int = BuildProof.answer
let hidden = BuildProof.hidden() // expected-error {{'hidden' is inaccessible due to 'internal' protection level}}
typealias Alias = BuildProof // expected-error {{cannot use namespace 'BuildProof' as a type}}
let value = BuildProof // expected-error {{expected namespace member name after namespace name}}

//--- wrapper.swift
import NamespaceLib

@inlinable public func wrappedAnswer() -> Int {
  BuildProof.answer()
}

//--- use.swift
import NamespaceWrapper
print(wrappedAnswer())
