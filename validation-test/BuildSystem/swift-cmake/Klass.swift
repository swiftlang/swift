// This isn't a test but lit wants to treat it as a test! Make it exit -1 to
// make sure lit doesn't run this file and add a REQUIRES line so we actually
// never do that.
//
// RUN: exit -1
// REQUIRES: not_a_test

public class Klass {}

public func useKlass(_ k: Klass) {}

@_cdecl("doSomething")
public func doSomething() -> Int {
    CPPLib_log()
    let x: Int = 34
    // Make sure we link against SwiftCore.
    print("Swift is going to return \(34)")
    return x
}
