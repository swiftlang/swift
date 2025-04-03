// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx14

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

extension View {
  func test() -> some View { EmptyView() }
}

extension ShapeStyle {
  @_disfavoredOverload
  func test() -> some ShapeStyle { Something() }
}

struct Something : ShapeStyle, View {
  var body: some View {
    EmptyView()
  }
}

struct TestOverlay1 : View {
  var body : some View {
    EmptyView().overlay(Something().test())
  }
}

struct TestOverlay2 : View {
  var body : some View {
    VStack {
      Group {
        Text("")
      }.overlay(Something().test())
    }
  }
}

struct TestBackground1 : View {
  var body : some View {
    EmptyView().background(Something().test())
  }
}

struct TestBackground2 : View {
  var body : some View {
    VStack {
      Group {
        Text("")
      }.background(Something().test())
    }
  }
}

struct TestDisfavoring : View {
  var body : some View {
    Something().test()
  }
}
