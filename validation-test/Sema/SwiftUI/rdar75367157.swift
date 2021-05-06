// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

enum E : String {
  case a
  case b

  var description: String { get { "" } }
}

struct S : View {
  var test: E = .a

  func check(_: String) -> Bool {}

  var body: some View {
    List {
      if test != E.b.description { // expected-error {{cannot convert value of type 'E' to expected argument type 'String'}} {{14-14=.rawValue}}
        EmptyView()
      }

      if (check(self.test)) { // expected-error {{cannot convert value of type 'E' to expected argument type 'String'}} {{26-26=.rawValue}}
        Spacer()
      }
    }
  }
}
