// RUN: %target-swift-frontend %s -target x86_64-apple-macosx10.15 -emit-sil -verify
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI
import Combine

struct A: View {
  var body: some View {
    Spacer()
  }
}

struct B: View {
  var body: some View {
    Spacer()
  }
}

class Context: ObservableObject {
  @State var flag: Bool

  init() {
    self.flag = false
  }
}

struct S : View {
  @EnvironmentObject var context: Context

  var body: some View {
    VStack {
      if (context.flag) { // Ok (shouldn't trip SIL Verifier)
        A()
      } else {
        B()
      }
    }
  }
}
