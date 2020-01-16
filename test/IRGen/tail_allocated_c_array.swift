// RUN: %target-swift-frontend -import-objc-header  %S/Inputs/tail_allocated_c_array.h -primary-file %s -emit-ir -o - | %FileCheck %s

// 25165828 = 0x1800004 The bottom bits designate the offset = 4
// CHECK: @keypath = private global <{{.*}}i32 0, i32 -2147483644, i32 25165828 }>

_ = MemoryLayout<foo>.offset(of: \foo.tailallocatedarray)!
