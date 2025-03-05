// REQUIRES: swift_swift_parser, swift_feature_ConcurrencySyntaxSugar

// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -enable-experimental-feature ConcurrencySyntaxSugar -language-mode 6 %s -dump-macro-expansions 2>&1 | %FileCheck %s

func f() async {}

// CHECK-LABEL: @__swiftmacro_10start_task4sync9StartTaskfMb_.swift
// CHECK: Task {
// CHECK:   await f()
// CHECK: }

@StartTask
func sync() {
  await f()
}



