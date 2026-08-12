// RUN: %target-swift-frontend -emit-sil -O %s | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D16ParameterOffset1ySis4SpanVySiG_S2itF :
// CHECK: cond_fail {{.*}}, "Index out of bounds"
// CHECK: // Loop header
// CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D16ParameterOffset1ySis4SpanVySiG_S2itF'
public func testParameterOffset1(_ span: Span<Int>, _ offset: Int, _ count: Int) -> Int {
    var sum = 0
    for i in 0..<count {
        sum &+= span[offset + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D16ParameterOffset2ySis4SpanVySiG_S2itF :
// TODO-CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D16ParameterOffset2ySis4SpanVySiG_S2itF'
public func testParameterOffset2(_ span: Span<Int>, _ offset: Int, _ count: Int) -> Int {
    precondition(offset >= 0 && offset + count <= span.count, "Valid range")
    var sum = 0
    for i in 0..<count {
        sum &+= span[offset + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D16ParameterOffset3ySis4SpanVySiG_S2itF :
// TODO-CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D16ParameterOffset3ySis4SpanVySiG_S2itF'
public func testParameterOffset3(_ span: Span<Int>, _ offset: Int, _ count: Int) -> Int {
    guard offset >= 0 && offset + count <= span.count else {
        fatalError("Invalid range")
    }
    var sum = 0
    for i in 0..<count {
        sum &+= span[offset + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D16LiteralConstant1ySis4SpanVySiG_SitF :
// CHECK: cond_fail {{.*}}, "Index out of bounds"
// CHECK: // Loop header
// CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D16LiteralConstant1ySis4SpanVySiG_SitF'
public func testLiteralConstant1(_ span: Span<Int>, _ count: Int) -> Int {
    var sum = 0
    for i in 0..<count {
        sum &+= span[5 + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D16LiteralConstant2ySis4SpanVySiG_SitF :
// TODO-CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D16LiteralConstant2ySis4SpanVySiG_SitF'
public func testLiteralConstant2(_ span: Span<Int>, _ count: Int) -> Int {
    precondition(5 + count <= span.count, "Valid range")
    var sum = 0
    for i in 0..<count {
        sum &+= span[5 + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D19DominatingComputed1ySis4SpanVySiG_S2itF
// CHECK: cond_fail {{.*}}, "Index out of bounds"
// CHECK: // Loop header
// CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D19DominatingComputed1ySis4SpanVySiG_S2itF'
public func testDominatingComputed1(_ span: Span<Int>, _ base: Int, _ count: Int) -> Int {
    let offset = base * 2 + 1
    var sum = 0
    for i in 0..<count {
        sum &+= span[offset + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D16LiteralConstant3ySis4SpanVySiG_SitF :
// TODO-CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D16LiteralConstant3ySis4SpanVySiG_SitF'
public func testLiteralConstant3(_ span: Span<Int>, _ count: Int) -> Int {
    guard 5 + count <= span.count else {
        fatalError("Invalid range")
    }
    var sum = 0
    for i in 0..<count {
        sum &+= span[5 + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D19DominatingComputed2ySis4SpanVySiG_S2itF
// TODO-CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D19DominatingComputed2ySis4SpanVySiG_S2itF'
public func testDominatingComputed2(_ span: Span<Int>, _ base: Int, _ count: Int) -> Int {
    let offset = base * 2 + 1
    precondition(offset >= 0 && offset + count <= span.count, "Valid range")
    var sum = 0
    for i in 0..<count {
        sum &+= span[offset + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D14NonDominating1ySis4SpanVySiG_SitF
// CHECK: // Loop header
// CHECK: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D14NonDominating1ySis4SpanVySiG_SitF'
public func testNonDominating1(_ span: Span<Int>, _ count: Int) -> Int {
    var sum = 0
    for i in 0..<count {
        let offset = i % 3
        sum &+= span[offset + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D19DominatingComputed3ySis4SpanVySiG_S2itF
// TODO-CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D19DominatingComputed3ySis4SpanVySiG_S2itF'
public func testDominatingComputed3(_ span: Span<Int>, _ base: Int, _ count: Int) -> Int {
    let offset = base * 2 + 1
    guard offset >= 0 && offset + count <= span.count else {
        fatalError("Invalid range")
    }
    var sum = 0
    for i in 0..<count {
        sum &+= span[offset + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D14NonDominating2ySis4SpanVySiG_SitF
// CHECK: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D14NonDominating2ySis4SpanVySiG_SitF'
public func testNonDominating2(_ span: Span<Int>, _ count: Int) -> Int {
    precondition(count * 2 < span.count, "Conservative bound")
    var sum = 0
    for i in 0..<count {
        let offset = i % 3
        sum &+= span[offset + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D15NegativeOffset1ySis4SpanVySiG_S3itF
// CHECK: // Loop header
// CHECK: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D15NegativeOffset1ySis4SpanVySiG_S3itF'
public func testNegativeOffset1(_ span: Span<Int>, _ start: Int, _ end: Int, _ offset: Int) -> Int {
    var sum = 0
    for i in start..<end {
        sum &+= span[i - offset]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D15NegativeOffset2ySis4SpanVySiG_S3itF
// TODO-CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D15NegativeOffset2ySis4SpanVySiG_S3itF'
public func testNegativeOffset2(_ span: Span<Int>, _ start: Int, _ end: Int, _ offset: Int) -> Int {
    precondition(start >= offset && end <= span.count, "Valid range")
    var sum = 0
    for i in start..<end {
        sum &+= span[i - offset]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D15NegativeOffset3ySis4SpanVySiG_S3itF
// TODO-CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D15NegativeOffset3ySis4SpanVySiG_S3itF'
public func testNegativeOffset3(_ span: Span<Int>, _ start: Int, _ end: Int, _ offset: Int) -> Int {
    guard start >= offset && end <= span.count else {
        fatalError("Invalid range")
    }
    var sum = 0
    for i in start..<end {
        sum &+= span[i - offset]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D21ConditionalExecution1ySis4SpanVySiG_S2iSbtF
// CHECK: // Loop header
// CHECK: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D21ConditionalExecution1ySis4SpanVySiG_S2iSbtF'
public func testConditionalExecution1(_ span: Span<Int>, _ offset: Int, _ count: Int, _ skipCondition: Bool) -> Int {
    var sum = 0
    for i in 0..<count {
        if skipCondition {
            continue
        }
        sum &+= span[offset + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D21ConditionalExecution2ySis4SpanVySiG_S2iSbtF
// TODO-CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D21ConditionalExecution2ySis4SpanVySiG_S2iSbtF'
public func testConditionalExecution2(_ span: Span<Int>, _ offset: Int, _ count: Int, _ skipCondition: Bool) -> Int {
    precondition(offset >= 0 && offset + count <= span.count, "Valid range")
    var sum = 0
    for i in 0..<count {
        if skipCondition {
            continue
        }
        sum &+= span[offset + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D21ConditionalExecution3ySis4SpanVySiG_S2iSbtF
// TODO-CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D21ConditionalExecution3ySis4SpanVySiG_S2iSbtF'
public func testConditionalExecution3(_ span: Span<Int>, _ offset: Int, _ count: Int, _ skipCondition: Bool) -> Int {
    guard offset >= 0 && offset + count <= span.count else {
        fatalError("Invalid range")
    }
    var sum = 0
    for i in 0..<count {
        if skipCondition {
            continue
        }
        sum &+= span[offset + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D15ParameterRange1ySis4SpanVySiG_S3itF :
// CHECK: cond_fail {{.*}}, "Index out of bounds"
// CHECK: // Loop header
// TODO-CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D15ParameterRange1ySis4SpanVySiG_S3itF'
public func testParameterRange1(_ span: Span<Int>, _ offset: Int, _ start: Int, _ end: Int) -> Int {
    var sum = 0
    for i in start...end {
        sum &+= span[offset + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D15ParameterRange2ySis4SpanVySiG_S3itF :
// TODO-CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D15ParameterRange2ySis4SpanVySiG_S3itF'
public func testParameterRange2(_ span: Span<Int>, _ offset: Int, _ start: Int, _ end: Int) -> Int {
    precondition(offset >= 0 && offset + end < span.count, "Valid range")
    var sum = 0
    for i in start...end {
        sum &+= span[offset + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D15ParameterRange3ySis4SpanVySiG_S3itF :
// TODO-CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D15ParameterRange3ySis4SpanVySiG_S3itF'
public func testParameterRange3(_ span: Span<Int>, _ offset: Int, _ start: Int, _ end: Int) -> Int {
    guard offset >= 0 && offset + end < span.count else {
        fatalError("Invalid range")
    }
    var sum = 0
    for i in start...end {
        sum &+= span[offset + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test25dontEliminateBoundsCheck1ySis4SpanVySiGF :
// CHECK: cond_fail {{.*}}, "Index out of bounds"
// CHECK: // Loop header
// CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test25dontEliminateBoundsCheck1ySis4SpanVySiGF'
public func dontEliminateBoundsCheck1(_ span: Span<Int>) -> Int {
    var sum = 0
    for i in 0..<span.count {
        sum += span[5 + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test25dontEliminateBoundsCheck2ySis4SpanVySiG_SitF :
// CHECK: cond_fail {{.*}}, "Index out of bounds"
// CHECK: // Loop header
// CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test25dontEliminateBoundsCheck2ySis4SpanVySiG_SitF'
public func dontEliminateBoundsCheck2(_ span: Span<Int>, _ offset: Int) -> Int {
    var sum = 0
    for i in 0..<span.count {
        sum += span[offset + i]
    }
    return sum
}

// CHECK-LABEL: sil @$s24bounds_check_offset_test0D6OffsetySis4SpanVySiG_S2itF :
// CHECK: cond_fail {{.*}}, "Index out of bounds"
// CHECK: // Loop header
// CHECK-NOT: cond_fail {{.*}}, "Index out of bounds"
// CHECK-LABEL: } // end sil function '$s24bounds_check_offset_test0D6OffsetySis4SpanVySiG_S2itF'
public func testOffset(_ v: borrowing Span<Int>, _ base: Int, _ n: Int) -> Int {
  var sum = 0
  guard base >= 0, n >= 0, base + n <= v.count else { return 0 }
  for i in 0..<n {
    sum &+= v[base + i]
  }
  return sum
}
