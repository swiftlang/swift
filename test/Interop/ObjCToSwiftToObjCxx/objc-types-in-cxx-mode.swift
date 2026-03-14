// RUN: %target-swift-frontend %s -module-name ObjCInCXX -typecheck -verify -I %S/Inputs -emit-clang-header-path %t/ObjCInCXX.h -I %t -cxx-interoperability-mode=default
// RUN: %FileCheck %s < %t/ObjCInCXX.h

// Make sure the header is valid when parsed in C++ language mode, without Obj-C.
// RUN: %target-interop-build-clang -fobjc-arc -c -x c++-header %t/ObjCInCXX.h -o %t/o.o

// REQUIRES: objc_interop

import ObjCTypes

public struct HasObjCInit {
  private let privateField: Int = 0

  public init(_ o: ObjCKlass) {}
}

public struct HasObjCMethod {
  private let privateField: Int = 0

  public func takesObjCKlass(_ o: ObjCKlass) {}
}

// CHECK:      SWIFT_INLINE_THUNK HasObjCInit HasObjCInit::init(ObjCKlass *_Nonnull o) {
// CHECK:        return {{.*}} {
// CHECK:        }
// CHECK-NEXT: }
// CHECK-NEXT: #endif // defined(__OBJC__)

// CHECK:      SWIFT_INLINE_THUNK void HasObjCMethod::takesObjCKlass(ObjCKlass *_Nonnull o) const {
// CHECK-NEXT:   ObjCInCXX::{{.*}}
// CHECK-NEXT: }
// CHECK-NEXT: #endif // defined(__OBJC__)
