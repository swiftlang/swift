// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx10.15 -swift-version 5 -disable-availability-checking
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

// Stand-in for SwiftUI's `@State`, which became a macro in recent SDKs and would
// otherwise require the SwiftUIMacros plugin. This test exercises type-checking
// performance, not `@State` itself; the box reproduces `@State`'s nonmutating
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

struct FullScreenView<Content>: View where Content: View {
  @FakeState private var showFullScreen: Bool = false

  var x: Double = 0
  var y: Double = 0

  var content: () -> Content

  // Body of this view used to cause rapid memory use.
  var body: some View {
    ZStack {
      VStack {
        HStack {
          Button {
          } label: {
            Text("a")
          }
          .offset(x: x, y: y)
        }
      }
    }
    .fullScreenCover(isPresented: $showFullScreen) {    
      ZStack {
        VStack {
          HStack {
            Button {
	      
            } label: {
              Text("a")
            }
          }
        }
      }
    }
  }
}
