// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test
// REQUIRES: OS=macosx

import Foundation

class c1: NSObject {
    let a = 99.0
    let b = 99
}

class c2 {
}

struct s1 {
}

class t: NSObject {
    let d: Double = 99.0
    let f: Float = 99.0
    let q: Int = 99
    let i: CInt = 99
    let c: Int8 = 99
    let b = true
    let s = ""
    let array = [String]()
    let dict = ["":1]
    let set = Set<String>()
    let w: () -> () = {}
    let x = c1()
    let y = c2()
    let z = s1()
}

var ic: UInt32 = 0
let ivars = class_copyIvarList( t.self, &ic )
for i in 0..<ic {
    print("\(String(validatingUTF8:ivar_getName(ivars[Int(i)]))!) \(String(validatingUTF8:ivar_getTypeEncoding(ivars[Int(i)]))!)")
}

// CHECK: d d
// CHECK: f f
// CHECK: q q
// CHECK: i i
// CHECK: c c
// CHECK: b c
// CHECK: s @"NSString"
// CHECK: array @"NSArray"
// CHECK: dict @"NSDictionary"
// CHECK: set @"NSSet"
// CHECK: w @?
// CHECK: x @"_TtC4main2c1"
// CHECK: y
// CHECK: z
