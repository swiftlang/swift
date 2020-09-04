// RUN: %target-typecheck-verify-swift -target x86_64-apple-macosx10.15 -swift-version 5
// REQUIRES: rdar66110075
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

struct ContentView : View {
  @State var foo: [String] = Array(repeating: "", count: 5)

  var body: some View {
    VStack { // expected-error{{type of expression is ambiguous without more context}}
      HStack {
        Text("")
        TextFi // expected-error {{cannot find 'TextFi' in scope}}
      }

      ForEach(0 ... 4, id: \.self) { i in
        HStack {
          TextField("", text: self.$foo[i])
        }
      }
    }
  }
}
