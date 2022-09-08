// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -I%platform-module-dir/../.. -L%platform-dylib-dir/../.. -emit-library -emit-library-path=%t/%target-library-name(MacroDefinition) -working-directory=%t -module-name=MacroDefinition %S/Inputs/macro_definition.swift
// RUN: %target-build-swift -L%platform-module-dir/../.. -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) %s -o %t/main
// RUN: %target-run %t/main
// REQUIRES: executable_test

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

let (result, code) = #customStringify(3 + 2 - 1)
print(result, code)
