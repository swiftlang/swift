
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 -g %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

import StdlibUnittest

var keyPathChecking = TestSuite("ReferencePrefixAppending")

class P3 {
    var name: String = "P3"
}
class P2 {
    var p3: P3 = P3()
}
class P1 {
    var p2: P2 = P2()
}

keyPathChecking.test("appendingReferenceWritable") {
    // KP1: \P1.p2 (ReferenceWritable)
    // Components: class(p2). hasRefPrefix=false.
    let kp1 = \P1.p2

    // KP2: \P2.p3 (ReferenceWritable)
    // Components: class(p3). hasRefPrefix=false.
    let kp2 = \P2.p3

    // KP3: \P1.p2.p3
    // Before fix: class(p2) has endOfRef=true. class(p3) has endOfRef=true.
    // After fix: class(p2) has endOfRef=false. class(p3) has endOfRef=true.
    let kp3 = kp1.appending(path: kp2)

    // KP4: \P3.name (ReferenceWritable)
    let kp4 = \P3.name

    // KP5: \P1.p2.p3.name
    // If kp3 has bug (two endOfRefs?), subsequent appends might propagate it or crash.
    let kp5 = kp3.appending(path: kp4)

    let p1 = P1()
    p1[keyPath: kp5] = "NewName"
    
    expectEqual(p1.p2.p3.name, "NewName")
}

runAllTests()
