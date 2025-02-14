// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil -Xllvm -sil-print-debuginfo %s \
// RUN:  | %FileCheck %s

struct MyStruct {
	var a = 12
}

public func use<T>(_ t : T){}

public func main() {
	var a = MyStruct() // line 11
  // Verify that the inserted struct_extract has the same location as the store.
  // CHECK:  %[[A:.*]] = apply {{.*}} -> MyStruct,
  // CHECK-SAME: loc {{.*}}:11:10, scope [[S:[0-9]+]]
  // CHECK-NEXT:  %[[I:.*]] = struct_extract %[[A]]
  // CHECK-SAME:  loc {{.*}}:11:10, scope [[S]]
  // CHECK:  store %[[A]] to %0 : $*MyStruct,
  // CHECK-SAME:  loc {{.*}}:11:10, scope [[S]]
	use(a.a)
}
