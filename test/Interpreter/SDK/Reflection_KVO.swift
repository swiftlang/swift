// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: sdk

// rdar://problem/19060227

import Foundation

class ObservedValue: NSObject {
	dynamic var amount = 0
}

class ValueObserver: NSObject {
	private var observeContext = 0
	let observedValue: ObservedValue
	
	init(value: ObservedValue) {
		observedValue = value
		super.init()
		observedValue.addObserver(self, forKeyPath: "amount", options: .New, context: &observeContext)
	}

	deinit {
		observedValue.removeObserver(self, forKeyPath: "amount")
	}
	
	override func observeValueForKeyPath(keyPath: String, ofObject object: AnyObject, change: [NSObject : AnyObject], context: UnsafeMutablePointer<Void>) {
		if context == &observeContext {
			if let amount = change[NSKeyValueChangeNewKey as NSString] as? Int {
				println("Observed value updated to \(amount)")
			}
		}
	}
}

let value = ObservedValue()
value.amount = 42
let observer = ValueObserver(value: value)
// CHECK: updated to 43
value.amount++
// CHECK: : 43
// FIXME: rdar://19537198 
// FIXME-CHECK: amount: 43
dump(value)


