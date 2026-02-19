// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated
// REQUIRES: objc_interop

import SwiftUI

Group {
    EmptyView()
    RoundedRectangle()  // expected-error {{missing argument for parameter 'cornerSize' in call}}
}
