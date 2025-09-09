// Make sure this can be compiled without any errors

// RUN: %target-swift-frontend %s -emit-ir -o /dev/null -enable-experimental-feature Embedded -enable-testing
// RUN: %target-swift-frontend %s -emit-ir -o /dev/null -enable-experimental-feature Embedded -enable-testing -no-allocations


// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

public protocol P {
  var storage: UInt32 { get set }
}

struct MyStruct: P {
  var storage: UInt32
}


