// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx12 -swift-version 5 -solver-scope-threshold=10000
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

// Invalid expression

struct MaskingView: View {
	@FakeState var xOffset: CGFloat = 0.0
	let count = 10
	
	var body: some View {
		ZStack {
			ForEach( 0 ..< count, id: \.self ) { index in
				let y: CGFloat = CGFloat(index * 20.0)  // expected-error {{cannot convert value of type 'Int' to expected argument type 'Double'}}
        // there's no * overload for (Int, Double)
				Circle()
					.fill((index%3==0) ? Color.white : Color.black)
					.frame(width: CGFloat(32 + index * 10), height: CGFloat(32 + index * 10))
					.offset(x: xOffset, y: y)
			}
		}
	}
}
