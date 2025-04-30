// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5

// REQUIRES: objc_interop
// REQUIRES: OS=macosx
// https://github.com/swiftlang/swift/issues/79255

import SwiftUI

struct ContentView : View {
  @State var foo: [String] = Array(repeating: "", count: 5)

  var body: some View {
    VStack {
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
