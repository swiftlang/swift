// Please keep this file in alphabetical order!

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules/ -F %S/Inputs/ -emit-module -o %t %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules/ -F %S/Inputs/ -parse-as-library %t/imports.swiftmodule -typecheck -emit-objc-header-path %t/imports.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/imports.h
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t/imports.h
// RUN: %check-in-clang %t/imports.h -I %S/Inputs/custom-modules/ -F %S/Inputs/ -Watimport-in-framework-header

// REQUIRES: objc_interop

// CHECK: #pragma clang diagnostic push

// CHECK: #if __has_warning("-Watimport-in-framework-header")
// CHECK-NEXT: #pragma clang diagnostic ignored "-Watimport-in-framework-header"
// CHECK-NEXT: #endif

// CHECK: @import Base;
// CHECK-NEXT: @import Base.ExplicitSub;
// CHECK-NEXT: @import Base.ExplicitSub.ExSub;
// CHECK-NEXT: @import Base.ImplicitSub.ExSub;
// CHECK-NEXT: @import Foundation;
// CHECK-NEXT: @import MostlyPrivate1;
// CHECK-NEXT: @import MostlyPrivate1_Private;
// CHECK-NEXT: @import MostlyPrivate2_Private;
// CHECK-NEXT: @import ctypes.bits;

// CHECK: #pragma clang diagnostic pop

// NEGATIVE-NOT: ctypes;
// NEGATIVE-NOT: ImSub;
// NEGATIVE-NOT: ImplicitSub;
// NEGATIVE-NOT: MostlyPrivate2;

import ctypes.bits
import Foundation

import Base
import Base.ImplicitSub
import Base.ImplicitSub.ImSub
import Base.ImplicitSub.ExSub
import Base.ExplicitSub
import Base.ExplicitSub.ImSub
import Base.ExplicitSub.ExSub

import MostlyPrivate1
import MostlyPrivate1_Private
// Deliberately not importing MostlyPrivate2
import MostlyPrivate2_Private

@objc class Test {
  @objc let word: DWORD = 0
  @objc let number: TimeInterval = 0.0

  @objc let baseI: BaseI = 0
  @objc let baseII: BaseII = 0
  @objc let baseIE: BaseIE = 0
  @objc let baseE: BaseE = 0
  @objc let baseEI: BaseEI = 0
  @objc let baseEE: BaseEE = 0

  // Deliberately use the private type before the public type.
  @objc let mp1priv: MP1PrivateType = 0
  @objc let mp1pub: MP1PublicType = 0

  @objc let mp2priv: MP2PrivateType = 0
}
