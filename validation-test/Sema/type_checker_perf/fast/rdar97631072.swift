// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-macosx12 -swift-version 5 -solver-scope-threshold=10000
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

// Invalid expression

struct MaskingView: View {
	@State var xOffset: CGFloat = 0.0
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
