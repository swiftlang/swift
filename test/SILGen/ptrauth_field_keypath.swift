// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %swift -emit-silgen -Xllvm -sil-print-types -target arm64e-apple-macos14.0 \
// RUN:   -import-objc-header %t/header.h %t/test.swift | %FileCheck %t/test.swift

// REQUIRES: CPU=arm64e
// REQUIRES: OS=macosx

// Key paths to __ptrauth fields must use accessors, not stored_property.

//--- header.h

#include <ptrauth.h>

struct Box {
  void *_Nullable __ptrauth(ptrauth_key_process_independent_data, 1, 1234) p;
  void (*_Nullable __ptrauth(ptrauth_key_function_pointer, 1, 5678) f)(void);
};

//--- test.swift

// CHECK-LABEL: sil {{.*}} @$s{{.*}}keyPathData{{.*}} :
// No `external`: Clang fields have no property descriptor.
// CHECK:   keypath {{.*}}, (root $Box; settable_property {{.*}} -> ()){{$| //}}
// CHECK-LABEL: } // end sil function
public func keyPathData() -> WritableKeyPath<Box, UnsafeMutableRawPointer?> {
  return \Box.p
}

// CHECK-LABEL: sil shared [thunk] {{.*}} @$sSo3BoxV1p{{.*}}TK :
// CHECK:   [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*Box, #Box.p
// CHECK:   [[ACC:%.*]] = begin_access [read] [signed] [[ADDR]]
// CHECK:   end_access [[ACC]]
// CHECK-LABEL: } // end sil function

// CHECK-LABEL: sil shared [thunk] {{.*}} @$sSo3BoxV1p{{.*}}Tk :
// CHECK:   [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*Box, #Box.p
// CHECK:   [[ACC:%.*]] = begin_access [modify] [signed] [[ADDR]]
// CHECK:   end_access [[ACC]]
// CHECK-LABEL: } // end sil function

// CHECK-LABEL: sil {{.*}} @$s{{.*}}keyPathFunction{{.*}} :
// No `external`: Clang fields have no property descriptor.
// CHECK:   keypath {{.*}}, (root $Box; settable_property {{.*}} -> ()){{$| //}}
// CHECK-LABEL: } // end sil function
public func keyPathFunction() -> WritableKeyPath<Box, (@convention(c) () -> Void)?> {
  return \Box.f
}

// CHECK-LABEL: sil shared [thunk] {{.*}} @$sSo3BoxV1f{{.*}}TK :
// CHECK:   [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*Box, #Box.f
// CHECK:   [[ACC:%.*]] = begin_access [read] [signed] [[ADDR]]
// CHECK:   end_access [[ACC]]
// CHECK-LABEL: } // end sil function

// CHECK-LABEL: sil shared [thunk] {{.*}} @$sSo3BoxV1f{{.*}}Tk :
// CHECK:   [[ADDR:%.*]] = struct_element_addr {{%.*}} : $*Box, #Box.f
// CHECK:   [[ACC:%.*]] = begin_access [modify] [signed] [[ADDR]]
// CHECK:   end_access [[ACC]]
// CHECK-LABEL: } // end sil function
