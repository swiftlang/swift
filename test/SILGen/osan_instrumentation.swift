// RUN: %target-swift-frontend -sanitize=optional -emit-silgen %s | %FileCheck %s
// XFAIL: linux

let val : Int? = nil
print(val!)
