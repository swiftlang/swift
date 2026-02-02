// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/opaque_circularity_swiftui_other.swift -emit-module -emit-module-path %t/opaque_circularity_swiftui_other.swiftmodule -target %target-cpu-apple-macosx12 -swift-version 5
// RUN: %target-swift-frontend -emit-silgen %s -I %t -target %target-cpu-apple-macosx12 -swift-version 5

// REQUIRES: OS=macosx

import opaque_circularity_swiftui_other
import SwiftUI

func foo(_ v: some View, _ tint: Color) {
  _ = v.foregroundStyle(tint)
}
