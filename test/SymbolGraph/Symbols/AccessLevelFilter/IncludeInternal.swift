// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/Internal.swift -module-name Internal -emit-module -emit-module-path %t/ -enable-testing
// RUN: %target-build-swift %s -module-name IncludeInternal -emit-module -emit-module-path %t/ -I %t
// RUN: %target-swift-symbolgraph-extract -module-name IncludeInternal -I %t -pretty-print -output-dir %t -minimum-access-level internal -emit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/IncludeInternal.symbols.json
// RUN: %FileCheck %s --input-file %t/IncludeInternal@Internal.symbols.json --check-prefix EXTENSION
// RUN: %FileCheck %s --input-file %t/IncludeInternal@Internal.symbols.json --check-prefix EXTENSION_EBS

public struct ShouldAppear {
  public var x: Int
}

internal struct ShouldAlsoAppear {
  internal var x: Int
}

private struct ShouldntAppear {
  var x: Int
}

// CHECK: ShouldAppear
// CHECK: ShouldAlsoAppear
// CHECK-NOT: ShouldntAppear 

@testable import Internal

extension S {
  func shouldAppear() { }
}

extension S {
  private func shouldntAppear() { }
}

// EXTENSION: shouldAppear
// EXTENSION-NOT: shouldntAppear
// EXTENSION_EBS-COUNT-1: "swift.extension"
