// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: OS=macosx

import SwiftUI

protocol TestLayout {}
extension TestLayout {
  func callAsFunction<V: View>(_ f: () -> V) -> some View {
    return f()
  }
}
struct EqualWidthHStack : TestLayout {}
extension EqualWidthHStack: View {
  var body : some View {
    Spacer()
  }
}

struct EmptyView: View {
  var body : some View { 
    Spacer()
  }
}

struct MyView: View {
  var body : some View {
    EqualWidthHStack {
      EmptyView()
    }
  }
}
