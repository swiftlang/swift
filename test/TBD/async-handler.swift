// RUN: %target-swift-frontend -enable-experimental-concurrency -enable-experimental-async-handler -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all %s

// REQUIRES: VENDOR=apple 
// REQUIRES: concurrency

open class TestClass {
  @asyncHandler
  internal func publicFunc() { }

  @asyncHandler
  internal func internalFunc() { }
}

@asyncHandler
public func globalFunc() { }
