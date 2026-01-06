// RUN: %target-swift-frontend -emit-sil %s -o /dev/null

// For OSLogTestHelper.
// REQUIRES: VENDOR=apple

import OSLogTestHelper

struct Thing {
  let guts: AnyObject
}

func getThings() -> [Thing] { [] }

func run() {
  var things: [Thing]
  while(true) {
    things = getThings()
    OSLogMessage("count: \(things.count)")
  }
}

