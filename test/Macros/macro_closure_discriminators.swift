//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// REQUIRES: swift_swift_parser, executable_test
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name MacroDefinition %t/MacroDefinition.swift -no-toolchain-stdlib-rpath

// AST verification can assign discriminators early and mask a collision between
// a macro expansion and a later closure in the same enclosing closure (#93626).
// RUN: %target-build-swift -swift-version 5 -Onone -Xfrontend -disable-ast-verifier -Xfrontend -sil-verify-all -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -swift-version 5 -O -Xfrontend -disable-ast-verifier -Xfrontend -sil-verify-all -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -swift-version 6 -Onone -Xfrontend -disable-ast-verifier -Xfrontend -sil-verify-all -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -swift-version 6 -O -Xfrontend -disable-ast-verifier -Xfrontend -sil-verify-all -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// Different closure abstraction patterns used to trigger a SIL verification error.
// RUN: %target-build-swift -swift-version 6 -DCONCRETE -Xfrontend -disable-ast-verifier -Xfrontend -sil-verify-all -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// Also check the AST verifier and repeated visits to nested closure bodies.
// RUN: %target-swift-frontend -swift-version 6 -emit-sil -enable-ast-verifier -sil-verify-all -load-plugin-library %t/%target-library-name(MacroDefinition) %t/main.swift -o /dev/null

//--- MacroDefinition.swift

import SwiftSyntax
import SwiftSyntaxMacros

struct IsEvenMacro: ExpressionMacro {
  static func expansion(
    of node: some FreestandingMacroExpansionSyntax,
    in context: some MacroExpansionContext
  ) throws -> ExprSyntax {
    "check(\(node.arguments.first!.expression)) { $0 % 2 == 0 }"
  }
}

//--- main.swift

@freestanding(expression)
macro isEven(_ value: Int) -> Bool = #externalMacro(module: "MacroDefinition", type: "IsEvenMacro")

func run(_ body: () -> Void) { body() }
func run(_ value: Int, _ body: (Int) -> Void) { body(value) }
func check<T>(_ value: T, _ predicate: (T) -> Bool) -> Bool { predicate(value) }

#if CONCRETE
func evaluate(_ value: Int, _ predicate: (Int) -> Bool) -> Bool { predicate(value) }
#else
func evaluate<T>(_ value: T, _ predicate: (T) -> Bool) -> Bool { predicate(value) }
#endif

func insideClosure() {
  var result = true
  run {
    _ = #isEven(2)
    result = evaluate(2) { $0 == 3 }
  }
  // CHECK: inside: false
  print("inside: \(result)")
}

func outsideClosure() {
  _ = #isEven(2)
  let result = evaluate(2) { $0 == 3 }
  // CHECK: outside: false
  print("outside: \(result)")
}

func nestedClosures() {
  var result = true
  run {
    run {
      _ = #isEven(2)
      result = evaluate(2) { $0 == 3 }
    }
  }
  // CHECK: nested: false
  print("nested: \(result)")
}

func parameterizedClosure() {
  var result = true
  run {
    run(2) { value in
      _ = #isEven(value)
      result = evaluate(value) { $0 == 3 }
    }
  }
  // CHECK: parameterized: false
  print("parameterized: \(result)")
}

func multipleExpansions() {
  var before = false
  var between = true
  var after = true
  run {
    before = evaluate(2) { $0 == 2 }
    _ = #isEven(2)
    between = evaluate(2) { $0 == 3 }
    _ = #isEven(2)
    after = evaluate(2) { $0 == 5 }
  }
  // CHECK: multiple: true false false
  print("multiple: \(before) \(between) \(after)")
}

insideClosure()
outsideClosure()
nestedClosures()
parameterizedClosure()
multipleExpansions()
