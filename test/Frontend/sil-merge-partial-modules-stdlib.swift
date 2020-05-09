// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -primary-file %s -module-name test -o %t/partial.swiftmodule -O

// RUN: %target-swift-frontend -emit-module %t/partial.swiftmodule -module-name test -sil-merge-partial-modules -disable-diagnostic-passes -disable-sil-perf-optzns -o %t/test.swiftmodule

public func makeMirror(object x: Any) -> Mirror {
  return Mirror(reflecting: x)
}
