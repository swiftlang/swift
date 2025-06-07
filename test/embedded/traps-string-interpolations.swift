// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -enable-experimental-feature Embedded -emit-ir -Osize -disable-llvm-merge-functions-pass | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

public func test1(i: Int) {
  fatalError("\(i) is not 42")
}

public func test2(i: Int) {
  assert(i == 42, "\(i) is not 42")
}

public func test3(i: Int) {
  precondition(i == 42, "\(i) is not 42")
}

public func test4(i: Int) {
  assertionFailure("\(i) is not 42")
}

public func test5(i: Int) {
  preconditionFailure("\(i) is not 42")
}

// CHECK:      define {{.*}}@"$e4main5test11iySi_tF"
// CHECK-NEXT: entry:
// CHECK-NEXT:   tail call void asm sideeffect ""
// CHECK-NEXT:   tail call void @llvm.trap()
// CHECK-NEXT:   unreachable
// CHECK-NEXT: }

// CHECK:      define {{.*}}@"$e4main5test21iySi_tF"
// CHECK-NEXT: entry:
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK:      define {{.*}}@"$e4main5test31iySi_tF"
// CHECK-NEXT: entry:
// CHECK-NEXT:   %.not = icmp eq i64 %0, 42
// CHECK-NEXT:   br i1 %.not, label %1, label %2
// CHECK-EMPTY: 
// CHECK-NEXT: 1:
// CHECK-NEXT:   ret void
// CHECK-EMPTY: 
// CHECK-NEXT: 2:
// CHECK-NEXT:   tail call void asm sideeffect ""
// CHECK-NEXT:   tail call void @llvm.trap()
// CHECK-NEXT:   unreachable
// CHECK-NEXT: }

// CHECK:      define {{.*}}@"$e4main5test41iySi_tF"
// CHECK-NEXT: entry:
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK:      define {{.*}}@"$e4main5test51iySi_tF"
// CHECK-NEXT: entry:
// CHECK-NEXT:   tail call void asm sideeffect ""
// CHECK-NEXT:   tail call void @llvm.trap()
// CHECK-NEXT:   unreachable
// CHECK-NEXT: }
