// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5 -enable-experimental-feature ResultBuilderASTTransform
// REQUIRES: OS=macosx

import SwiftUI

protocol TestLayout {}
extension TestLayout {
  func callAsFunction<V: View>(_ f: () -> V) -> some View {
    return f()
  }
}
struct EqualWitdthHStack : TestLayout {}
extension EqualWitdthHStack: View {
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
    EqualWitdthHStack {
      EmptyView()
    }
  }
}
