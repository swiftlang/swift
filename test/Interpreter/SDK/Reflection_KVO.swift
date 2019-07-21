// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

// rdar://problem/19060227

import Foundation

class ObservedValue: NSObject {
	@objc dynamic var amount = 0
}

class ValueObserver: NSObject {
	private var observeContext = 0
	let observedValue: ObservedValue

	init(value: ObservedValue) {
		observedValue = value
		super.init()
		observedValue.addObserver(self, forKeyPath: "amount", options: .new, context: &observeContext)
	}

	deinit {
		observedValue.removeObserver(self, forKeyPath: "amount")
	}

	override func observeValue(forKeyPath keyPath: String?, of object: Any?, change: [NSKeyValueChangeKey : Any]?, context: UnsafeMutableRawPointer?) {
		if context == &observeContext {
      if let change_ = change {
        if let amount = change_[.newKey] as? Int {
          print("Observed value updated to \(amount)")
        }
      }
		}
	}
}

let value = ObservedValue()
value.amount = 42
let observer = ValueObserver(value: value)
// CHECK: updated to 43
value.amount += 1
// CHECK: amount: 43
dump(value)


