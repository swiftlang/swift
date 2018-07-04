// RUN: %target-run-simple-swift

// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation
import Dispatch

var WithoutEscapingSuite = TestSuite("WithoutActuallyEscapingBlock")

var sink: Any = ()

@objc class BlockConsumer : NSObject {

  @inline(never)
  @objc dynamic func call(block: @escaping () -> ()) {
    block()
  }

}

func dontReallyEscape(f: @convention(block) () -> ()) {
  let escape : (@escaping @convention(block) () -> ()) -> () = { (b: @escaping @convention(block) () -> ()) -> () in
    BlockConsumer().call(block : b)
  }
  let _ :() = withoutActuallyEscaping(f, do: escape)
}


WithoutEscapingSuite.test("ExpectNoCrash") {
  var shouldBeTrue = false
  dontReallyEscape(f: { shouldBeTrue=true })
  expectTrue(shouldBeTrue)
}

WithoutEscapingSuite.test("ExpectNoCrash2") {
  for _ in 1...10 {
    let queue = DispatchQueue(label: "Foo")
    queue.sync { }
  }
}

runAllTests()
