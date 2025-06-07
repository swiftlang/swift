// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Macros

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift \
// RUN:   -swift-version 5 -o %t/broken-plugin \
// RUN:   -module-name=TestPlugin \
// RUN:   %t/broken_plugin.swift

// RUN: not %target-swift-frontend \
// RUN:   -typecheck \
// RUN:   -swift-version 5 -enable-experimental-feature Macros \
// RUN:   -load-plugin-executable %t/broken-plugin#TestPlugin \
// RUN:   -module-name MyApp \
// RUN:   -serialize-diagnostics-path %t/macro_expand.dia \
// RUN:   %t/test.swift

// RUN: c-index-test -read-diagnostics %t/macro_expand.dia 2>&1 | %FileCheck -check-prefix CHECK %s

// CHECK: test.swift:1:33: warning: external macro implementation type 'TestPlugin.FooMacro' could not be found for macro 'fooMacro'; '{{.*}}broken-plugin' produced malformed response
// CHECK: test.swift:4:7: error: external macro implementation type 'TestPlugin.FooMacro' could not be found for macro 'fooMacro'; '{{.*}}broken-plugin' produced malformed response
// CHECK: +-{{.+}}test.swift:1:33: note: 'fooMacro' declared here

//--- test.swift
@freestanding(expression) macro fooMacro(_: Any) -> String = #externalMacro(module: "TestPlugin", type: "FooMacro")

func test() {
  _ = #fooMacro(1)
}

//--- broken_plugin.swift
func minusTen(value: UInt) -> UInt {
  // Causes crash.
  return value - 10
}

_ = minusTen(value: 5)
