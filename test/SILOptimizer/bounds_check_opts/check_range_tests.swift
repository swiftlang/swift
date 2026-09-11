// RUN: %target-swift-frontend %s -emit-sil -O -disable-availability-checking | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir -O -disable-availability-checking | %FileCheck %s --check-prefix=CHECK-IR

// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib

// CHECK-LABEL: sil {{.*}}@$s17check_range_tests6sum_u8ySis7RawSpanVF :
// CHECK: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: // Loop header
// CHECK-NOT: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: cond_br
// CHECK-LABEL: } // end sil function '$s17check_range_tests6sum_u8ySis7RawSpanVF'

// CHECK-IR-LABEL: define {{.*}} @"$s17check_range_tests6sum_u8ySis7RawSpanVF"
// CHECK-IR: @llvm.vector.reduce.add
public func sum_u8(_ span: RawSpan) -> Int {
    var total = 0
    for i in 0..<span.byteCount {
        total &+= Int(span.load(fromByteOffset: i, as: UInt8.self))
    }
    return total
}

// CHECK-LABEL: sil {{.*}}@$s17check_range_tests7sum_u32ySis7RawSpanVF :
// CHECK: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: // Loop header
// CHECK-NOT: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: cond_br
// CHECK-LABEL: } // end sil function '$s17check_range_tests7sum_u32ySis7RawSpanVF'

// CHECK-IR-LABEL: define {{.*}} @"$s17check_range_tests7sum_u32ySis7RawSpanVF"
// CHECK-IR: @llvm.vector.reduce.add
public func sum_u32(_ span: RawSpan) -> Int {
    precondition(span.byteCount >= MemoryLayout<UInt32>.size)
    var total = 0
    for i in 0..<(span.byteCount - MemoryLayout<UInt32>.size + 1) {
        total &+= Int(span.load(fromByteOffset: i, as: UInt32.self))
    }
    return total
}

// CHECK-LABEL: sil {{.*}}@$s17check_range_tests16sum_invariant_u8_2at_Sis7RawSpanV_S2itF :
// CHECK: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: // Loop header
// CHECK-NOT: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: cond_br
// CHECK-LABEL: } // end sil function '$s17check_range_tests16sum_invariant_u8_2at_Sis7RawSpanV_S2itF'

// CHECK-IR-LABEL: define {{.*}} @"$s17check_range_tests16sum_invariant_u8_2at_Sis7RawSpanV_S2itF"
// CHECK-IR-NOT: @llvm.vector.reduce.add
public func sum_invariant_u8(_ span: RawSpan, at offset: Int, _ count: Int) -> Int {
    var total = 0
    for _ in 0..<count {
        total &+= Int(span.load(fromByteOffset: offset, as: UInt8.self))
    }
    return total
}

// CHECK-LABEL: sil {{.*}}@$s17check_range_tests15sum_conditionalySis7RawSpanV_SbtF :
// CHECK: // Loop header
// CHECK: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: cond_br
// CHECK-LABEL: } // end sil function '$s17check_range_tests15sum_conditionalySis7RawSpanV_SbtF'

// CHECK-IR-LABEL: define {{.*}} @"$s17check_range_tests15sum_conditionalySis7RawSpanV_SbtF"
// CHECK-IR-NOT: @llvm.vector.reduce.add
public func sum_conditional(_ span: RawSpan, _ flag: Bool) -> Int {
    var total = 0
    for i in 0..<span.byteCount {
        if flag {
            total &+= Int(span.load(fromByteOffset: i, as: UInt8.self))
        }
    }
    return total
}

// TODO: Support hoisting the range checks here.
// CHECK-LABEL: sil {{.*}}@$s17check_range_tests10sum_offset_4baseSis7RawSpanV_SitF :
// CHECK: // Loop header
// CHECK: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: cond_br
// CHECK-LABEL: } // end sil function '$s17check_range_tests10sum_offset_4baseSis7RawSpanV_SitF'
public func sum_offset(_ span: RawSpan, base: Int) -> Int {
    var total = 0
    for i in 0..<span.byteCount {
        total &+= Int(span.load(fromByteOffset: i + base, as: UInt8.self))
    }
    return total
}

// TODO: Support hoisting the range checks here.
// CHECK-LABEL: sil {{.*}}@$s17check_range_tests28sum_unknown_loop_bounds_u8_1ySis7RawSpanV_S2itF :
// CHECK: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: // Loop header
// CHECK: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: cond_br
// CHECK-LABEL: } // end sil function '$s17check_range_tests28sum_unknown_loop_bounds_u8_1ySis7RawSpanV_S2itF'

// CHECK-IR-LABEL: define {{.*}} @"$s17check_range_tests28sum_unknown_loop_bounds_u8_1ySis7RawSpanV_S2itF"
// CHECK-IR: @llvm.vector.reduce.add
public func sum_unknown_loop_bounds_u8_1(_ span: RawSpan, _ lower: Int, _ upper: Int) -> Int {
    var total = 0
    for i in lower...upper {
        total &+= Int(span.load(fromByteOffset: i, as: UInt8.self))
    }
    return total
}

// CHECK-LABEL: sil {{.*}}@$s17check_range_tests28sum_unknown_loop_bounds_u8_2ySis7RawSpanV_S2itF :
// CHECK: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: // Loop header
// CHECK-NOT: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: cond_br
// CHECK-LABEL: } // end sil function '$s17check_range_tests28sum_unknown_loop_bounds_u8_2ySis7RawSpanV_S2itF'

// CHECK-IR-LABEL: define {{.*}} @"$s17check_range_tests28sum_unknown_loop_bounds_u8_2ySis7RawSpanV_S2itF"
// CHECK-IR: @llvm.vector.reduce.add
public func sum_unknown_loop_bounds_u8_2(_ span: RawSpan, _ lower: Int, _ n: Int) -> Int {
    var total = 0
    for i in lower..<n {
        total &+= Int(span.load(fromByteOffset: i, as: UInt8.self))
    }
    return total
}

// CHECK-LABEL: sil {{.*}}@$s17check_range_tests16sum_u8_with_trapySis7RawSpanVF :
// CHECK: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: // Loop header
// CHECK-NOT: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: cond_br
// CHECK-LABEL: } // end sil function '$s17check_range_tests16sum_u8_with_trapySis7RawSpanVF'

// CHECK-IR-LABEL: define {{.*}} @"$s17check_range_tests16sum_u8_with_trapySis7RawSpanVF"
// CHECK-IR-NOT: @llvm.vector.reduce.add
public func sum_u8_with_trap(_ span: RawSpan) -> Int {
    var total = 0
    for i in 0..<span.byteCount {
        total += Int(span.load(fromByteOffset: i, as: UInt8.self))
    }
    return total
}

// CHECK-LABEL: sil {{.*}}@$s17check_range_tests17sum_u32_with_trapySis7RawSpanVF :
// CHECK: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: // Loop header
// CHECK-NOT: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: cond_br
// CHECK-LABEL: } // end sil function '$s17check_range_tests17sum_u32_with_trapySis7RawSpanVF'

// CHECK-IR-LABEL: define {{.*}} @"$s17check_range_tests17sum_u32_with_trapySis7RawSpanVF"
// CHECK-IR-NOT: @llvm.vector.reduce.add
public func sum_u32_with_trap(_ span: RawSpan) -> Int {
    precondition(span.byteCount >= MemoryLayout<UInt32>.size)
    var total = 0
    for i in 0..<(span.byteCount - MemoryLayout<UInt32>.size + 1) {
        total += Int(span.load(fromByteOffset: i, as: UInt32.self))
    }
    return total
}

// TODO: Support hoisting the range checks here.
// CHECK-LABEL: sil {{.*}}@$s17check_range_tests38sum_unknown_loop_bounds_u8_1_with_trapySis7RawSpanV_S2itF :
// CHECK: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: // Loop header
// CHECK: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: cond_br
// CHECK-LABEL: } // end sil function '$s17check_range_tests38sum_unknown_loop_bounds_u8_1_with_trapySis7RawSpanV_S2itF'

// CHECK-IR-LABEL: define {{.*}} @"$s17check_range_tests38sum_unknown_loop_bounds_u8_1_with_trapySis7RawSpanV_S2itF"
// CHECK-IR-NOT: @llvm.vector.reduce.add
public func sum_unknown_loop_bounds_u8_1_with_trap(_ span: RawSpan, _ lower: Int, _ upper: Int) -> Int {
    var total = 0
    for i in lower...upper {
        total += Int(span.load(fromByteOffset: i, as: UInt8.self))
    }
    return total
}

// CHECK-LABEL: sil {{.*}}@$s17check_range_tests38sum_unknown_loop_bounds_u8_2_with_trapySis7RawSpanV_S2itF :
// CHECK: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: // Loop header
// CHECK-NOT: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: cond_br
// CHECK-LABEL: } // end sil function '$s17check_range_tests38sum_unknown_loop_bounds_u8_2_with_trapySis7RawSpanV_S2itF'

// CHECK-IR-LABEL: define {{.*}} @"$s17check_range_tests38sum_unknown_loop_bounds_u8_2_with_trapySis7RawSpanV_S2itF"
// CHECK-IR-NOT: @llvm.vector.reduce.add
public func sum_unknown_loop_bounds_u8_2_with_trap(_ span: RawSpan, _ lower: Int, _ n: Int) -> Int {
    var total = 0
    for i in lower..<n {
        total += Int(span.load(fromByteOffset: i, as: UInt8.self))
    }
    return total
}

// CHECK-LABEL: sil {{.*}}@$s17check_range_tests8store_u8yys14MutableRawSpanVzF :
// CHECK: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: // Loop header
// CHECK-NOT: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: cond_br
// CHECK-LABEL: } // end sil function '$s17check_range_tests8store_u8yys14MutableRawSpanVzF'

// CHECK-IR-LABEL: define {{.*}} @"$s17check_range_tests8store_u8yys14MutableRawSpanVzF"
// CHECK-IR: vector.body
public func store_u8(_ span: inout MutableRawSpan) {
    for i in 0..<span.byteCount {
        span.storeBytes(of: UInt8(truncatingIfNeeded: i), toByteOffset: i, as: UInt8.self)
    }
}

// CHECK-LABEL: sil {{.*}}@$s17check_range_tests9store_u32yys14MutableRawSpanVzF :
// CHECK: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: // Loop header
// CHECK-NOT: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: cond_br
// CHECK-LABEL: } // end sil function '$s17check_range_tests9store_u32yys14MutableRawSpanVzF'

// CHECK-IR-LABEL: define {{.*}} @"$s17check_range_tests9store_u32yys14MutableRawSpanVzF"
// CHECK-IR: vector.body
public func store_u32(_ span: inout MutableRawSpan) {
    precondition(span.byteCount >= MemoryLayout<UInt32>.size)
    for i in 0..<(span.byteCount - MemoryLayout<UInt32>.size + 1) {
        span.storeBytes(of: UInt32(truncatingIfNeeded: i), toByteOffset: i, as: UInt32.self)
    }
}

// TODO: Support hoisting the range checks here.
// CHECK-LABEL: sil {{.*}}@$s17check_range_tests30store_unknown_loop_bounds_u8_1yys14MutableRawSpanVz_S2itF :
// CHECK: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: // Loop header
// CHECK: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: cond_br
// CHECK-LABEL: } // end sil function '$s17check_range_tests30store_unknown_loop_bounds_u8_1yys14MutableRawSpanVz_S2itF'

// CHECK-IR-LABEL: define {{.*}} @"$s17check_range_tests30store_unknown_loop_bounds_u8_1yys14MutableRawSpanVz_S2itF"
// CHECK-IR: vector.body
public func store_unknown_loop_bounds_u8_1(_ span: inout MutableRawSpan, _ lower: Int, _ upper: Int) {
    for i in lower...upper {
        span.storeBytes(of: UInt8(truncatingIfNeeded: i), toByteOffset: i, as: UInt8.self)
    }
}

// CHECK-LABEL: sil {{.*}}@$s17check_range_tests30store_unknown_loop_bounds_u8_2yys14MutableRawSpanVz_S2itF :
// CHECK: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: // Loop header
// CHECK-NOT: cond_fail {{.*}}, "Byte offset range out of bounds"
// CHECK: cond_br
// CHECK-LABEL: } // end sil function '$s17check_range_tests30store_unknown_loop_bounds_u8_2yys14MutableRawSpanVz_S2itF'

// CHECK-IR-LABEL: define {{.*}} @"$s17check_range_tests30store_unknown_loop_bounds_u8_2yys14MutableRawSpanVz_S2itF"
// CHECK-IR: vector.body
public func store_unknown_loop_bounds_u8_2(_ span: inout MutableRawSpan, _ lower: Int, _ n: Int) {
    for i in lower..<n {
        span.storeBytes(of: UInt8(truncatingIfNeeded: i), toByteOffset: i, as: UInt8.self)
    }
}
