// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

extension HorizontalAlignment {
  private enum MyHorizontalAlignment: AlignmentID {
    static func defaultValue(in d: ViewDimensions) -> CGFloat { d.width }
  }
}

extension EnvironmentValues {
  var myHorizontalAlignment: AlignmentID? {
    get { fatalError() }
    set { self[\.MyHorizontalAlignmentEnvironmentKey.self] = newValue }
    // expected-error@-1 {{value of type 'EnvironmentValues' has no member 'MyHorizontalAlignmentEnvironmentKey'}}
    // expected-error@-2 {{missing argument label 'keyPath:' in subscript}}
  }
}

struct MyHorizontalAlignmentEnvironmentKey: EnvironmentKey {
  static var defaultValue: AlignmentID?
}
