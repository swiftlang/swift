// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_MacrosOnImports

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/../../../Macros/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-swift-frontend -swift-version 5 -emit-module -o %t/macro_decl.swiftmodule %t/macro_decl.swift -module-name macro_decl -load-plugin-library %t/%target-library-name(MacroDefinition)
// RUN: %target-swift-frontend -swift-version 5 -typecheck -verify -cxx-interoperability-mode=default -enable-experimental-feature MacrosOnImports -load-plugin-library %t/%target-library-name(MacroDefinition) -I %t -I %t/Inputs %t/client.swift

//--- macro_decl.swift
@attached(extension)
public macro EmptyExtension() = #externalMacro(module: "MacroDefinition", type: "EmptyExtensionMacro")

//--- Inputs/module.modulemap
module CxxAPI {
    header "cxx.h"
    requires cplusplus
    export *
}

//--- Inputs/cxx.h
#pragma once

struct __attribute__((swift_attr("@macro_decl.EmptyExtension"))) Thing {};

// Two types that carry the macro and reference each other, to make sure
// expanding the macro on one does not cycle through the other.
struct MutualCaller1;
struct __attribute__((swift_attr("@macro_decl.EmptyExtension"))) MutualCaller2 {
  MutualCaller1 *foo;
  void bar(MutualCaller1 *mc);
};
struct __attribute__((swift_attr("@macro_decl.EmptyExtension"))) MutualCaller1 {
  MutualCaller2 baz;
  void qux(MutualCaller2 *mc);
};

//--- client.swift
import CxxAPI
import macro_decl

func use() {
  let t: Thing? = nil
  _ = t
  let a: MutualCaller1? = nil
  _ = a
  let b: MutualCaller2? = nil
  _ = b
}
