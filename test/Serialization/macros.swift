// REQUIRES: asserts

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t-scratch)
// RUN: %target-build-swift -I %swift-host-lib-dir -L %swift-host-lib-dir -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/def_macro_plugin.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-swift-frontend -emit-module -o %t/def_macros.swiftmodule %S/Inputs/def_macros.swift -module-name def_macros -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir
// RUN: %target-swift-frontend -typecheck -I%t -verify %s -verify-ignore-unknown  -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir
// REQUIRES: OS=macosx

import def_macros

func test(a: Int, b: Int) {
  _ = #publicStringify(a + b)

  _ = #internalStringify(a + b)
  // expected-error@-1{{no macro named 'internalStringify'}}
}

struct TestStruct {
  @myWrapper var x: Int
}
