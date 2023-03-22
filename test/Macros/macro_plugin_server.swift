// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/plugins)
//
//== Build the plugin library
// RUN: %target-build-swift \
// RUN:   -swift-version 5 \
// RUN:   -I %swift-host-lib-dir \
// RUN:   -L %swift-host-lib-dir \
// RUN:   -emit-library \
// RUN:   -o %t/plugins/%target-library-name(MacroDefinition) \
// RUN:   -module-name=MacroDefinition \
// RUN:   %S/Inputs/syntax_macro_definitions.swift \
// RUN:   -g -no-toolchain-stdlib-rpath

// RUN: %target-build-swift \
// RUN:   -swift-version 5 \
// RUN:   -I %swift-host-lib-dir \
// RUN:   -L %swift-host-lib-dir \
// RUN:   -emit-library \
// RUN:   -o %t/plugins/%target-library-name(EvilMacros) \
// RUN:   -module-name=EvilMacros \
// RUN:   %S/Inputs/evil_macro_definitions.swift \
// RUN:   -g -no-toolchain-stdlib-rpath

// RUN: %swift-target-frontend \
// RUN:   -typecheck -verify \
// RUN:   -swift-version 5 \
// RUN:   -external-plugin-path %t/plugins#%swift-plugin-server \
// RUN:   -dump-macro-expansions \
// RUN:   %s \
// RUN:   2>&1 | tee %t/macro-expansions.txt

// RUN: %FileCheck -strict-whitespace %s < %t/macro-expansions.txt


@freestanding(expression) macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")
@freestanding(expression) macro evil(_ value: Int) -> String = #externalMacro(module: "EvilMacros", type: "CrashingMacro")

func testStringify(a: Int, b: Int) {
  let s1: String = #stringify(a + b).1
  print(s1)

  let s2: String = #evil(42) // expected-error {{failedToReceiveMessage (from macro 'evil')}}
  print(s2)

  let s3: String = #stringify(b + a).1
  print(s3)
}

// CHECK:      {{^}}------------------------------
// CHECK-NEXT: {{^}}(a + b, "a + b")
// CHECK-NEXT: {{^}}------------------------------

// CHECK:      {{^}}------------------------------
// CHECK-NEXT: {{^}}(b + a, "b + a")
// CHECK-NEXT: {{^}}------------------------------
