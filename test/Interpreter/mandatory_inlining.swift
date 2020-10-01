// RUN: %target-run-simple-swift

// REQUIRES: executable_test

// This file contains various scenarios that validate that various functional
// behaviors of mandatory inlining work as expected. Please give each test a
// descriptive name and a large comment explaining what you are testing.

import StdlibUnittest

var Tests = TestSuite("MandatoryInlining Functional Tests")
defer { runAllTests() }

func doNotInsertReleaseInLoopIfValueConsumedInLoop(
  callBack: ((String) -> (Void))? = nil) {
  let callBack = callBack ?? { msg in
    print(msg)
  }

  for _ in 0..<1 {
    callBack("foo bar baz")
  }
}

// When using the linear lifetime checker to insert compensating releases, if we
// find a double consume due to a loop, do not insert an apply at that call site
// that is in the loop. There is already a compensating retain in the loop that
// we are ignoring. This test functionally makes sure we no longer do this.
Tests.test("doNotInsertReleaseInLoopIfValueConsumedInLoop") {
  doNotInsertReleaseInLoopIfValueConsumedInLoop { msg in
      print(msg)
  }
}


@propertyWrapper
public struct Published<Value> {
    public var wrappedValue: Value
    public init(initialValue value: Value) { wrappedValue = value }
}

private class Model {
    var selectedContact: Int? {
        get { _selectedContact }
        set {
            if let contact = newValue {
                self._selectedContact = contact
            }
        }
    }

    @Published private var _selectedContact: Int? = nil
}

Tests.test("testNonOSSAPartialApplyLifetimeExtension") {
  // If we mess this up, when we set model.selectedContact to nil, we release
  // self incorrectly due to mandatory inlining getting the wrong begin lifetime
  // block (which in this case is the entry block).
  let model = Model()
  model.selectedContact = nil
}
