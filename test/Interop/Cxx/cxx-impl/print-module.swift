// Check round-trip through a .swiftmodule: both plain @cxx and @cxx("...").

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -I %S/Inputs \
// RUN:   -emit-module %s \
// RUN:   -module-name CxxImplPrint \
// RUN:   -o %t/CxxImplPrint.swiftmodule
// RUN: %target-swift-ide-test \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -I %S/Inputs \
// RUN:   -I %t \
// RUN:   -print-module \
// RUN:   -module-to-print=CxxImplPrint \
// RUN:   -source-filename=x | %FileCheck %s

// REQUIRES: swift_feature_CxxImplementation

import Functions

// CHECK: @cxx{{$}}
// CHECK-NEXT: @implementation func existingDeclaration1(_ param: Int32) -> Int32
@cxx @implementation
public func existingDeclaration1(_ param: Int32) -> Int32 {
  return param
}

// CHECK: @cxx("existingDeclaration3")
// CHECK-NEXT: @implementation func existingDeclaration3Alias(_ param: Int32) -> Int32
@cxx("existingDeclaration3") @implementation
public func existingDeclaration3Alias(_ param: Int32) -> Int32 {
  return param
}
