// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5
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
    // expected-error@-1 {{generic parameter 'K' could not be inferred}}
    // expected-error@-2 {{cannot infer key path type from context; consider explicitly specifying a root type}}
    // expected-error@-3 {{cannot convert value of key path type to expected argument type 'K.Type'}}
  }
}

struct MyHorizontalAlignmentEnvironmentKey: EnvironmentKey {
  static var defaultValue: AlignmentID?
}
