// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)
// RUN: %target-build-swift -I %swift-host-lib-dir -L %swift-host-lib-dir -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-swift-frontend -emit-module -o %t/macro_library.swiftmodule %S/Inputs/macro_library.swift -module-name macro_library -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir
// RUN: %target-swift-frontend -typecheck -I%t -verify -primary-file %s %S/Inputs/macro_expand_other.swift -verify-ignore-unknown  -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir

// REQUIRES: asserts
// REQUIRES: OS=macosx

import macro_library

struct Treat {}

@Observable
final class Dog: Observable {
  var name: String?
  var treat: Treat?

  var isHappy: Bool = true

  init() {}

  func bark() {
    print("bork bork")
  }
}

func test() {
  observeDog()
}
