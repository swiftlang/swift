// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -import-ns-enum -triple x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-llvm | FileCheck %s

import Foundation
import gizmo

// TODO: metadata and type descriptor for NSRuncingOptions emitted with 
// linkonce_odr hidden constant linkage

// CHECK: define i16 @_T12objc_ns_enum22imported_enum_inject_aFT_OSC16NSRuncingOptions()
def imported_enum_inject_a() -> NSRuncingOptions {
  // TODO: Enum constructors for imported enums aren't yet emitted by SILGen.
  return .NSRuncingMince
}
