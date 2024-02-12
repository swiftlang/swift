// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5 -import-objc-header %S/Inputs/move_only_types_cycle.h
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// This test triggered a cyclic dependency between @objc getter selector
// checking and some checking for move-only types, which is both horrifying and
// exciting at the same time.

import Foundation

@objc(CPTricksyClass)
public class TricksyClass: NSObject {
  public enum Color {
    case red
  }

  var id = UUID()
}

extension TricksyClass.Color {
}
