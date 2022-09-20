// RUN: %target-swift-frontend -target %target-cpu-apple-macosx10.15 -swift-version 5 -c %s
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import SwiftUI

struct DropDestinationCoordinatorView: NSViewRepresentable {
	func makeNSView(context: Context) -> some NSView {
    return NSView()
	}
	
	func updateNSView(_ nsView: NSViewType, context: Context) {
		print("for view:  \(nsView)")
	}
}

