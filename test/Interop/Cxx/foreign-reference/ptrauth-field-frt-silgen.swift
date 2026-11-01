// RUN: %swift %s -emit-silgen -Xllvm -sil-print-types -target arm64e-apple-macos14.0 \
// RUN:   -cxx-interoperability-mode=default -I %S/Inputs/ | %FileCheck %s

// REQUIRES: CPU=arm64e
// REQUIRES: OS=macosx

// Regression test for rdar://190639894:
// Swift was ignoring __ptrauth qualifiers on fields of C++ foreign reference
// types, emitting begin_access [dynamic] instead of begin_access [signed].
// This caused ptrauth-qualified data pointer fields (such as xnu's
// OS_PTRAUTH_SIGNED_PTR fields) to be accessed without the required
// autda/pacda instructions, breaking pointer authentication.

import PtrAuthFieldFRT

// --- Read from a ptrauth field on a foreign reference type ---
// The field access must use begin_access [signed], not [dynamic].

// CHECK-LABEL: sil {{.*}} @$s{{.*}}readFRT{{.*}}F :
// CHECK:   [[ADDR:%.*]] = ref_element_addr {{%.*}} : $RefBox, #RefBox.p
// CHECK:   [[ACC:%.*]] = begin_access [read] [signed] [[ADDR]]
// CHECK:   end_access [[ACC]]
// CHECK-LABEL: } // end sil function
public func readFRT(_ b: RefBox) -> Int32 {
  return unsafe b.p!.pointee
}

// --- Write to a ptrauth field on a foreign reference type ---
// The field access must use begin_access [modify] [signed].

// CHECK-LABEL: sil {{.*}} @$s{{.*}}writeFRT{{.*}}F :
// CHECK:   [[ADDR:%.*]] = ref_element_addr {{%.*}} : $RefBox, #RefBox.p
// CHECK:   [[ACC:%.*]] = begin_access [modify] [signed] [[ADDR]]
// CHECK:   end_access [[ACC]]
// CHECK-LABEL: } // end sil function
public func writeFRT(_ b: RefBox, _ q: UnsafeMutablePointer<Int32>) {
  unsafe b.p = q
}

// --- Control: a plain (non-ptrauth) field on the same FRT ---
// A plain field must NOT use begin_access [signed].

// CHECK-LABEL: sil {{.*}} @$s{{.*}}plainFRT{{.*}}F :
// CHECK:   ref_element_addr {{%.*}} : $RefBox, #RefBox.plain
// CHECK-NOT: begin_access [read] [signed]
// CHECK-NOT: begin_access [modify] [signed]
// CHECK-LABEL: } // end sil function
public func plainFRT(_ b: RefBox) -> Int32 {
  return b.plain
}

// --- Control: a ptrauth field on a plain C struct (already worked) ---
// Verify the struct path still produces [signed] (regression guard).

// CHECK-LABEL: sil {{.*}} @$s{{.*}}readCBox{{.*}}F :
// CHECK:   [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*CBox, #CBox.p
// CHECK:   [[ACC:%.*]] = begin_access [read] [signed] [[ADDR]]
// CHECK:   end_access [[ACC]]
// CHECK-LABEL: } // end sil function
public func readCBox(_ b: UnsafePointer<CBox>) -> Int32 {
  return unsafe b.pointee.p!.pointee
}

// --- Read from an address-only C struct value ---
// The rvalue load path must also use begin_access [signed].

// CHECK-LABEL: sil {{.*}} @$s{{.*}}readCBoxByValue{{.*}}F :
// CHECK:   [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*CBox, #CBox.p
// CHECK:   [[ACC:%.*]] = begin_access [read] [signed] [[ADDR]]
// CHECK:   end_access [[ACC]]
// CHECK-LABEL: } // end sil function
public func readCBoxByValue(_ b: CBox) -> UnsafeMutablePointer<Int32>? {
  return unsafe b.p
}

// --- Key paths to ptrauth fields ---
// Key paths must not use a stored_property component (which the runtime
// projects by raw field offset, without authentication). They must use
// accessor thunks that go through begin_access [signed]. Clang-imported
// fields have no property descriptor, so there is no `external` reference.

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
