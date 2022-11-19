// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -I %swift-lib-dir -L %swift-lib-dir -emit-library -emit-library-path=%t/%target-library-name(MacroDefinition) -working-directory=%t -module-name=MacroDefinition %S/Inputs/macro_definition.swift
// RUN: %target-build-swift -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-lib-dir -L %swift-lib-dir %s -o %t/main
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

macro customStringify<T>(_ value: T) -> (T, String) = MacroDefinition.StringifyMacro

print(#customStringify(3 + 2 - 1))
// CHECK: (4, "3 + 2 - 1")

print(#customStringify(1.0.truncatingRemainder(dividingBy: 1.0) + 3.0))
// CHECK-NEXT: (3.0, "1.0.truncatingRemainder(dividingBy: 1.0) + 3.0")

print(#customStringify(["a", "b", "c"] + ["d", "e", "f"]))
// CHECK-NEXT: (["a", "b", "c", "d", "e", "f"], "[\"a\", \"b\", \"c\"] + [\"d\", \"e\", \"f\"]")
