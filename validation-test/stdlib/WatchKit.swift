// RUN: %target-run-simple-swift

// REQUIRES: objc_interop
// REQUIRES: OS=watchos

import StdlibUnittest
import WatchKit

var WatchKitTests = TestSuite("WatchKit")

if #available(iOS 8.2, *) {
// This is a very weak test, but we can't do better without spawning a GUI app.
WatchKitTests.test("WKInterfaceController/reloadRootControllers(_:)") {
  WKInterfaceController.reloadRootControllers([])
}

// This is a very weak test, but we can't do better without spawning a GUI app.
WatchKitTests.test("WKInterfaceController/presentController(_:)") {
  var curried = WKInterfaceController.presentController
  typealias ExpectedType =
    (WKInterfaceController) -> ([(name: String, context: AnyObject)]) -> ()
  let checkType: ExpectedType = curried
  _blackHole(checkType)

  // FIXME: can't write the following line: rdar://20985062
  // expectType(ExpectedType.self, &curried)
}

} // #available(iOS 8.2, *)

runAllTests()

