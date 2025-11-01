// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop

import SwiftUI

Group {
    EmptyView()
    RoundedRectangle()  // expected-error {{missing argument for parameter 'cornerSize' in call}}
}
