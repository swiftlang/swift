// REQUIRES: swift_swift_parser
// REQUIRES: executable_test

// RUN: %empty-directory(%t)

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-build-swift-dylib(%t/%target-library-name(Serialized)) %S/macro_default_argument_library.swift -module-name Serialized -emit-module -emit-module-path %t/Serialized.swiftmodule -load-plugin-library %t/%target-library-name(MacroDefinition) -I %t -swift-version 5

// RUN: %target-build-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %s -o %t/main -module-name MacroUser -emit-tbd -emit-tbd-path %t/MacroUser.tbd -I %t -L %t %target-rpath(%t) -lSerialized
// RUN: %target-codesign %t/main
// RUN: %target-codesign %t/%target-library-name(Serialized)
// RUN: %target-run %t/main %t/%target-library-name(Serialized) | %FileCheck %s

import Serialized

// CHECK: ("MacroUser/macro_default_argument_serialized.swift", "#fileID")
print(foo())
