// Please keep this file in alphabetical order!

// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend %clang-importer-sdk -I %S/Inputs/custom-modules/ -emit-module -o %t %s
// RUN: %target-swift-frontend %clang-importer-sdk -I %S/Inputs/custom-modules/ -parse-as-library %t/imports.swiftmodule -parse -emit-objc-header-path %t/imports.h -import-objc-header %S/../Inputs/empty.h
// RUN: FileCheck %s < %t/imports.h
// RUN: FileCheck -check-prefix=NEGATIVE %s < %t/imports.h
// RUN: %check-in-clang %t/imports.h -I %S/Inputs/custom-modules/

// CHECK-DAG: @import ctypes.bits;
// CHECK-DAG: @import Foundation;
// CHECK-DAG: @import Base;
// CHECK-DAG: @import Base.ImplicitSub.ExSub;
// CHECK-DAG: @import Base.ExplicitSub;
// CHECK-DAG: @import Base.ExplicitSub.ExSub;

// NEGATIVE-NOT: ctypes;
// NEGATIVE-NOT: ImSub;
// NEGATIVE-NOT: ImplicitSub;

import ctypes.bits
import Foundation

import Base
import Base.ImplicitSub;
import Base.ImplicitSub.ImSub;
import Base.ImplicitSub.ExSub;
import Base.ExplicitSub;
import Base.ExplicitSub.ImSub;
import Base.ExplicitSub.ExSub;

@objc class Test {
  let word: DWORD = 0
  let number: NSTimeInterval = 0.0

  let baseI: BaseI = 0
  let baseII: BaseII = 0
  let baseIE: BaseIE = 0
  let baseE: BaseE = 0
  let baseEI: BaseEI = 0
  let baseEE: BaseEE = 0
}
