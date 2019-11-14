// RUN: %target-swift-frontend -emit-ir -sanitize=address -sanitize-recover=address %s | %FileCheck %s -check-prefix=ASAN_RECOVER
// RUN: %target-swift-frontend -emit-ir -sanitize=address  %s | %FileCheck %s -check-prefix=ASAN_NO_RECOVER
// RUN: %target-swift-frontend -emit-ir -sanitize-recover=address %s | %FileCheck %s -check-prefix=NO_ASAN_RECOVER

// ASAN_RECOVER: declare void @__asan_loadN_noabort(
// ASAN_NO_RECOVER: declare void @__asan_loadN(

let size:Int = 128;
let x = UnsafeMutablePointer<UInt8>.allocate(capacity: size)
x.initialize(repeating: 0, count: size)
x.advanced(by: 0).pointee = 5;
print("Read first element:\(x.advanced(by: 0).pointee)")
x.deallocate();

// There should be no ASan instrumentation in this case.
// NO_ASAN_RECOVER-NOT: declare void @__asan_load
