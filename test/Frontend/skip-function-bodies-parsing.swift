// RUN: %empty-directory(%t/stats)

// RUN: %target-swift-frontend -emit-module -experimental-skip-non-inlinable-function-bodies-without-types -module-name Mod -emit-module-path %t/Mod.swiftmodule -stats-output-dir %t/stats %s
// RUN: %{python} %utils/process-stats-dir.py --set-csv-baseline %t/stats.csv %t/stats
// RUN: %FileCheck -input-file %t/stats.csv %s

// The printing implementation differs in asserts and no-asserts builds, it will
// either print `"Parse.NumFunctionsParsed" 0` or not print it at all. Make sure
// we don't output any non-zero value.
// CHECK-NOT: {{"Parse.NumFunctionsParsed" [^0]}}

// Make sure we skip parsing these bodies.
public func foo(x: Int, y: Int) {}

public func bar() {
  func baz() {}
}

public struct S {
  public func qux() {}
}
