// RUN: %target-swift-frontend -emit-sil -enable-lifetime-resolution -sil-verify-all -verify -module-name main %s \
// RUN:   -Xllvm -sil-print-after=diagnose-resolved-lifetimes -o /dev/null 2>&1 | %FileCheck --enable-var-scope %s

// SILGen copies borrowed noncopyable values. Those copies only used as borrows are removed.

class Klass {}

// CHECK-LABEL: sil hidden [transparent] {{.*}}@$s4main2NCV1kAA5KlassCvg :
// CHECK:       [[K:%.*]] = struct_extract {{%.*}}, #NC.k
// CHECK-NEXT:  copy_value [[K]]
// CHECK-NOT:   copy_value
// CHECK-LABEL: } // end sil function '$s4main2NCV1kAA5KlassCvg'

struct NC: ~Copyable {
  var k: Klass

  // CHECK-LABEL: sil hidden {{.*}}@$s4main2NCV6methodyyF :
  // CHECK-NOT:   copy_value
  // CHECK-LABEL: } // end sil function '$s4main2NCV6methodyyF'
  func method() {}
}

func useNC(_ n: borrowing NC) {}
func consumeKlass(_ k: consuming Klass) {}

// CHECK-LABEL: sil hidden {{.*}}@$s4main14borrowedMethodyyAA2NCVF :
// CHECK-NOT:   copy_value
// CHECK-LABEL: } // end sil function '$s4main14borrowedMethodyyAA2NCVF'
func borrowedMethod(_ n: borrowing NC) { n.method() }

// CHECK-LABEL: sil hidden {{.*}}@$s4main11borrowedArgyyAA2NCVF :
// CHECK-NOT:   copy_value
// CHECK-LABEL: } // end sil function '$s4main11borrowedArgyyAA2NCVF'
func borrowedArg(_ n: borrowing NC) { useNC(n) }

// Copying a Copyable field of a borrowed noncopyable value is permitted.
// CHECK-LABEL: sil hidden {{.*}}@$s4main28consumeBorrowedCopyableFieldyyAA2NCVF :
// CHECK:       [[K:%.*]] = struct_extract {{%.*}}, #NC.k
// CHECK-NEXT:  [[C:%.*]] = copy_value [[K]]
// CHECK:       apply {{%.*}}([[C]])
// CHECK-NOT:   copy_value
// CHECK-LABEL: } // end sil function '$s4main28consumeBorrowedCopyableFieldyyAA2NCVF'
func consumeBorrowedCopyableField(_ n: borrowing NC) { consumeKlass(n.k) }

// CHECK-LABEL: sil hidden {{.*}}@$s4main15borrowedGenericyyx_yxXEtRi_zlF :
// CHECK-NOT:   copy_value
// CHECK-LABEL: } // end sil function '$s4main15borrowedGenericyyx_yxXEtRi_zlF'
func borrowedGeneric<T: ~Copyable>(_ t: borrowing T, _ f: (borrowing T) -> ()) { f(t) }
