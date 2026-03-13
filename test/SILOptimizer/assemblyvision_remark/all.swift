// RUN: %target-swift-frontend -enable-assembly-vision-all -emit-sil %s -Osize -o - -module-name main 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -enable-assembly-vision-all -emit-sil %s -Osize -o - -module-name main 2>&1 | %FileCheck %s --check-prefix=REMARK

public class C {

  // CHECK: sil [transparent] [_semantics "optremark"] @$s4main1CC1iSivg
  public var i: Int = 0

  // CHECK: sil hidden [_semantics "optremark"] @$s4main1CCACycfc
  init() {
      print("\(i)")
  }

  // CHECK: sil [_semantics "optremark"] @$s4main1CC6methodSiyF
  public func method() -> Int {
// REMARK: 17:14: remark: begin exclusive access to value of type 'Int'
      return i
// REMARK: 17:14: remark: end exclusive access to value of type 'Int'
  }

  // CHECK: sil [_semantics "optremark"] @$s4main1CCfd
}

// CHECK sil [_semantics "optremark"] @$s4main12freestandingAA1CCyF
public func freestanding() -> C {
    return C()
}
