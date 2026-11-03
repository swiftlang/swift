// RUN: %swift %s -emit-silgen -Xllvm -sil-print-types -target arm64e-apple-macos14.0 \
// RUN:   -cxx-interoperability-mode=default -I %S/Inputs/ | %FileCheck %s

// REQUIRES: CPU=arm64e
// REQUIRES: OS=macosx

import PtrAuthFieldFRT

// CHECK-LABEL: sil {{.*}} @$s{{.*}}readFRT{{.*}}F :
// CHECK:   [[ADDR:%.*]] = ref_element_addr {{%.*}} : $RefBox, #RefBox.p
// CHECK:   [[ACC:%.*]] = begin_access [read] [signed] [[ADDR]]
// CHECK:   end_access [[ACC]]
// CHECK-LABEL: } // end sil function
public func readFRT(_ b: RefBox) -> Int32 {
  return unsafe b.p!.pointee
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}writeFRT{{.*}}F :
// CHECK:   [[ADDR:%.*]] = ref_element_addr {{%.*}} : $RefBox, #RefBox.p
// CHECK:   [[ACC:%.*]] = begin_access [modify] [signed] [[ADDR]]
// CHECK:   end_access [[ACC]]
// CHECK-LABEL: } // end sil function
public func writeFRT(_ b: RefBox, _ q: UnsafeMutablePointer<Int32>) {
  unsafe b.p = q
}

// Plain fields must not get [signed].
// CHECK-LABEL: sil {{.*}} @$s{{.*}}plainFRT{{.*}}F :
// CHECK:   ref_element_addr {{%.*}} : $RefBox, #RefBox.plain
// CHECK-NOT: begin_access [read] [signed]
// CHECK-NOT: begin_access [modify] [signed]
// CHECK-LABEL: } // end sil function
public func plainFRT(_ b: RefBox) -> Int32 {
  return b.plain
}

// C struct field to guard against regressions.
// CHECK-LABEL: sil {{.*}} @$s{{.*}}readCBox{{.*}}F :
// CHECK:   [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*CBox, #CBox.p
// CHECK:   [[ACC:%.*]] = begin_access [read] [signed] [[ADDR]]
// CHECK:   end_access [[ACC]]
// CHECK-LABEL: } // end sil function
public func readCBox(_ b: UnsafePointer<CBox>) -> Int32 {
  return unsafe b.pointee.p!.pointee
}

// CHECK-LABEL: sil {{.*}} @$s{{.*}}readCBoxByValue{{.*}}F :
// CHECK:   [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*CBox, #CBox.p
// CHECK:   [[ACC:%.*]] = begin_access [read] [signed] [[ADDR]]
// CHECK:   end_access [[ACC]]
// CHECK-LABEL: } // end sil function
public func readCBoxByValue(_ b: CBox) -> UnsafeMutablePointer<Int32>? {
  return unsafe b.p
}

// Key path literal must use settable_property (accessor thunks), not stored_property.
// CHECK-LABEL: sil {{.*}} @$s{{.*}}keyPathFRT{{.*}}F :
// CHECK:   keypath $ReferenceWritableKeyPath<RefBox, Optional<UnsafeMutablePointer<Int32>>>, (root $RefBox; settable_property $Optional<UnsafeMutablePointer<Int32>>,{{ +}}id ##RefBox.p, getter @[[GETTER:\$sSo6RefBoxV1p[^ ]*TK]] : {{.*}}, setter @[[SETTER:\$sSo6RefBoxV1p[^ ]*Tk]] : {{.*}} -> ()){{$| //}}
// CHECK-LABEL: } // end sil function
public func keyPathFRT() -> ReferenceWritableKeyPath<RefBox, UnsafeMutablePointer<Int32>?> {
  return \RefBox.p
}

// CHECK: sil shared [thunk] {{.*}} @[[GETTER]] :
// CHECK:   [[ADDR:%.*]] = ref_element_addr {{%.*}} : $RefBox, #RefBox.p
// CHECK:   [[ACC:%.*]] = begin_access [read] [signed] [[ADDR]]
// CHECK:   end_access [[ACC]]
// CHECK-LABEL: } // end sil function

// CHECK: sil shared [thunk] {{.*}} @[[SETTER]] :
// CHECK:   [[ADDR:%.*]] = ref_element_addr {{%.*}} : $RefBox, #RefBox.p
// CHECK:   [[ACC:%.*]] = begin_access [modify] [signed] [[ADDR]]
// CHECK:   end_access [[ACC]]
// CHECK-LABEL: } // end sil function

// CHECK-LABEL: sil {{.*}} @$s{{.*}}keyPathCBox{{.*}} :
// CHECK:   keypath {{.*}}, (root $CBox; settable_property
// CHECK-NOT: stored_property
// CHECK-LABEL: } // end sil function
public func keyPathCBox() -> WritableKeyPath<CBox, UnsafeMutablePointer<Int32>?> {
  return \CBox.p
}

// CHECK-LABEL: sil shared [thunk] {{.*}} @$sSo4CBoxV1p{{.*}}TK :
// CHECK:   [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*CBox, #CBox.p
// CHECK:   [[ACC:%.*]] = begin_access [read] [signed] [[ADDR]]
// CHECK:   end_access [[ACC]]
// CHECK-LABEL: } // end sil function

// CHECK-LABEL: sil shared [thunk] {{.*}} @$sSo4CBoxV1p{{.*}}Tk :
// CHECK:   [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*CBox, #CBox.p
// CHECK:   [[ACC:%.*]] = begin_access [modify] [signed] [[ADDR]]
// CHECK:   end_access [[ACC]]
// CHECK-LABEL: } // end sil function

// CHECK-LABEL: sil private {{.*}} @$s{{.*}}makeReader{{.*}}fU_ :
// CHECK:   [[ADDR:%.*]] = ref_element_addr {{%.*}} : $RefBox, #RefBox.p
// CHECK:   [[ACC:%.*]] = begin_access [read] [signed] [[ADDR]]
// CHECK:   end_access [[ACC]]
// CHECK-LABEL: } // end sil function
public func makeReader(_ b: RefBox) -> () -> UnsafeMutablePointer<Int32>? {
  return { unsafe b.p }
}
