// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

struct Configuration {
}

struct Reader<Content: View> : View {
  struct Result {
    var view: AnyView?
  }

  var content: (Result) -> (Content)

  var body: some View { EmptyView() }
}

struct MyLayout {
  func callAsFunction<V: View>(@ViewBuilder content: () -> V) -> some View { content() }
}

struct Test<SubView: View>: View {
  var subView: SubView
  var body: some View { EmptyView() }
}

func test() -> some View {
  Reader { result in
    MyLayout().callAsFunction {
      Test(subView: result.view)
    }
  }
}
