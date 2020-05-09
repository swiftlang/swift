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

if #available(OSX 10.10, iOS 8.0, *) {

enum Result<T> {
  case error
  case t(T)
}

func foo<T>(block: () throws -> T) {
  let q = DispatchQueue(label: "Test")
  var result: Result<T>! = nil
  withoutActuallyEscaping(block) { (block) -> () in
    let item = DispatchWorkItem(qos: .unspecified, flags: []) {
      do {
        result = .t(try block())
      }
      catch {
        result = .error
      }
    }
    q.sync(execute: item)
  }
}

WithoutEscapingSuite.test("ExpectNoCrash3") {
  for _ in 1...10 {
    foo(block: { return 10 })
  }
}

}
runAllTests()
