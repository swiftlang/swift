// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// Too complex with Xcode 16.2 SDK
// REQUIRES: rdar141262107

import SwiftUI

enum E {
  case a, b, c
}

struct S : View {
  let values: [E] = [.a, .b, .c]

  var body: some View {
    ScrollView(.vertical) {
      Group {
        Group {
          ForEach(values, id: \.self) { color in
            Button(action: { labeled(true) }) { // expected-error {{missing argument label 'value:' in call}} {{38-38=value: }}
              Text("").bold()
            }.buttonStyle(BorderlessButtonStyle())
          }

          ForEach([1, 2, 3, 4, 5, 6, 7, 8, 9], id: \.self) { _ in
            Button(action: { labeled(value: true) }) {
              Text("").bold()
            }.buttonStyle(BorderlessButtonStyle())
          }
        }
        .frame(width: 100, height: 100)
        .padding(.top, 1)
      }
    }
    .frame(width: 100)
  }

  func labeled(value: Bool) {}
}
