// RUN: %target-swift-frontend %s -target %target-cpu-apple-macosx10.15 -emit-sil -verify
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI
import Combine

// Stand-in for SwiftUI's `@State`, which became a macro in recent SDKs and would
// otherwise require the SwiftUIMacros plugin. This test exercises the SIL
// verifier, not `@State` itself; the box reproduces `@State`'s nonmutating
// setter and `Binding` projected value.
@propertyWrapper
struct FakeState<Value> {
  final class Box { var value: Value; init(_ value: Value) { self.value = value } }
  private let box: Box
  init(wrappedValue: Value) { box = Box(wrappedValue) }
  var wrappedValue: Value {
    get { box.value }
    nonmutating set { box.value = newValue }
  }
  var projectedValue: Binding<Value> {
    Binding(get: { box.value }, set: { box.value = $0 })
  }
}

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
  @FakeState var flag: Bool

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
