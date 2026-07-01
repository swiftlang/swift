// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx14 -swift-version 5 -solver-scope-threshold=100000
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// Invalid expressions

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

struct SubView: View {
    var body: some View {
        EmptyView()
    }
}

struct TestView: View {
    @FakeState private var value: (row: Int, column: Int)? = nil
    
    var body: some View {
        Grid {
            ForEach(1...3, id: \.self) { row in
                GridRow {
                    ForEach(1...3, id: \.self) { column in
                        SubView()  // expected-error {{reasonable time}}
                            .onTapGesture {
                                value = (row, column)
                            }
                            // error: you have to unwrap the optional
                            .scaleEffect(value == (row, column) ? 1.5 : 1.0)
                            .foregroundStyle(value == (row, column) ? .red : .tint)
                            .font(.system(size: value == (row, column) ? 50 : 30))
                    }
                }
            }
        }
    }
}
