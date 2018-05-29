// RUN: %empty-directory(%t)
//
// RUN: %target-clang -fobjc-arc %S/Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: %target-build-swift -O -I %S/Inputs/ObjCClasses/ -Xlinker %t/ObjCClasses.o %s -o %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

import ObjCClasses
import Foundation

protocol OuterProto:class {
}

protocol InnerProto : class {
    var outerthings:[OuterProto] { get }
}

extension OuterType : OuterProto {
}

extension OuterType.InnerType: InnerProto {
    var outerthings:[OuterProto] {
        return self.things
    }
}

var innerthing:InnerProto = OuterType.InnerType()

@inline(never)
func getInnerThing() -> InnerProto {
  return innerthing
}

@inline(never)
func dontCrash() {
  let thing = getInnerThing()
  let devices = thing.outerthings
  print("Got devices: \(devices)")
}

// CHECK: Got devices: []
dontCrash()
