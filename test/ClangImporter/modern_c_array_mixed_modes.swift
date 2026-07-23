// Test that library code using the modern C array projection can be inlined
// into clients that use the legacy projection, and vice versa. Doing this
// requires that both projections be imported (even if one is hidden) and that
// SIL serialization not mix them up.

// REQUIRES: swift_feature_ModernImportedCArrays
// REQUIRES: executable_test

// RUN: %empty-directory(%t/include)
// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/fragile)
// RUN: %empty-directory(%t/resilient)
// RUN: split-file %s %t

//
// Checks for SIL output:
// * legacyFunc() and modernFunc() are emitted into clients and can coexist.
// * legacyFunc() should always use the legacy projection.
// * modernFunc() should always use the modern projection.
//

// SIL-LABEL: sil shared {{.*}}@$s9LegacyLib10legacyFuncyyF
// SIL: [[BIT_CAST_VALUE:%.*]] = unchecked_trivial_bit_cast {{%.*}} to $InlineArray<4, Int32>
// SIL-NEXT: struct $S ([[BIT_CAST_VALUE]])
// SIL: keypath $WritableKeyPath
// SIL-SAME: <S, (Int32, Int32, Int32, Int32)>, (root $S; settable_property $(Int32, Int32, Int32, Int32),  id @$sSo1SV5arrays5Int32V_A3Etvg : {{.*}}, getter @$sSo1SV5arrays5Int32V_A3EtvpABTKq : {{.*}}, setter @$sSo1SV5arrays5Int32V_A3EtvpABTkq : {{.*}})
// SIL: } // end sil function '$s9LegacyLib10legacyFuncyyF'

// SIL-LABEL: sil shared {{.*}}@$s9ModernLib10modernFuncyyF
// SIL: [[ALLOC_STACK_VALUE:%.*]] = alloc_stack $InlineArray<4, Int32>
// SIL: [[LOAD_VALUE:%.*]] = load [[ALLOC_STACK_VALUE]]
// SIL: struct $S ([[LOAD_VALUE]])
// SIL: keypath $WritableKeyPath
// SIL-SAME: <S, InlineArray<4, Int32>>, (root $S; stored_property #S.array : {{.*}})
// SIL: } // end sil function '$s9ModernLib10modernFuncyyF'

//
// Checks for execution output
//

// OUTPUT: legacyFunc:
// OUTPUT-NEXT: S 0 1 2 3
// OUTPUT-NEXT: modernFunc:
// OUTPUT-NEXT: S 0 1 2 3
// OUTPUT-NEXT: main:
// OUTPUT-NEXT: S 0 1 2 3

//--- include/CLib.h

struct S {
  int array[4];
};

void print_s(const struct S *ptr);

//--- include/module.modulemap

module CLib {
  header "CLib.h"
  export *
}

//--- src/LegacyLib.swift

// RUN: %target-swift-frontend %t/src/LegacyLib.swift -I %t/include -c -module-name LegacyLib -o %t/fragile/LegacyLib.o -emit-module-path %t/fragile/LegacyLib.swiftmodule -parse-as-library -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple

// RUN: %target-swift-frontend %t/src/LegacyLib.swift -I %t/include -c -module-name LegacyLib -o %t/resilient/LegacyLib.o -emit-module-path %t/resilient/LegacyLib.swiftmodule -parse-as-library -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple -emit-module-interface-path %t/resilient/LegacyLib.swiftinterface -enable-library-evolution

import CLib

@export(implementation) public func legacyFunc() {
  var s = S(array: (.max, 1, 2, 3))
  s.array.0 = 0
  blackHole(\S.array)
  print_s(&s)
}

@inline(never) @usableFromInline internal func blackHole(_: some Any) {}

@c(print_s) @implementation public func print_s(_ ptr: UnsafePointer<S>!) {
  print("S", ptr.pointee.array.0, ptr.pointee.array.1, ptr.pointee.array.2, ptr.pointee.array.3)
}

//--- src/ModernLib.swift

// RUN: %target-swift-frontend %t/src/ModernLib.swift -I %t/include -c -module-name ModernLib -o %t/fragile/ModernLib.o -emit-module-path %t/fragile/ModernLib.swiftmodule -parse-as-library -module-cache-path %t/module-cache -swift-version 5 -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple

// RUN: %target-swift-frontend %t/src/ModernLib.swift -I %t/include -c -module-name ModernLib -o %t/resilient/ModernLib.o -emit-module-path %t/resilient/ModernLib.swiftmodule -parse-as-library -module-cache-path %t/module-cache -swift-version 5 -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -emit-module-interface-path %t/resilient/ModernLib.swiftinterface -enable-library-evolution

import CLib

@export(implementation) public func modernFunc() {
  var s = S(array: [.max, 1, 2, 3])
  s.array[0] = 0
  blackHole(\S.array)
  print_s(&s)
}

@inline(never) @usableFromInline internal func blackHole(_: some Any) {}

//--- src/LegacyExec.swift

// RUN: %target-build-swift -emit-sil %t/src/LegacyExec.swift -g -o %t/fragile/LegacyExec.sil -I %t/include -I %t/fragile -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple
// RUN: %FileCheck --input-file %t/fragile/LegacyExec.sil --check-prefix SIL %s
// RUN: %target-build-swift -emit-executable %t/src/LegacyExec.swift %t/fragile/LegacyLib.o %t/fragile/ModernLib.o -g -o %t/fragile/LegacyExec -I %t/include -I %t/fragile -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple
// RUN: %target-codesign %t/fragile/LegacyExec
// RUN: %target-run %t/fragile/LegacyExec > %t/fragile/LegacyExec.txt
// RUN: %FileCheck --input-file %t/fragile/LegacyExec.txt --check-prefix OUTPUT %s

// RUN: %target-build-swift -emit-sil %t/src/LegacyExec.swift -g -o %t/resilient/LegacyExec.sil -I %t/include -I %t/resilient -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple
// RUN: %FileCheck --input-file %t/resilient/LegacyExec.sil --check-prefix SIL %s
// RUN: %target-build-swift -emit-executable %t/src/LegacyExec.swift %t/resilient/LegacyLib.o %t/resilient/ModernLib.o -g -o %t/resilient/LegacyExec -I %t/include -I %t/resilient -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple
// RUN: %target-codesign %t/resilient/LegacyExec
// RUN: %target-run %t/resilient/LegacyExec > %t/resilient/LegacyExec.txt
// RUN: %FileCheck --input-file %t/resilient/LegacyExec.txt --check-prefix OUTPUT %s

import CLib
import LegacyLib
import ModernLib

print("legacyFunc:")
legacyFunc()

print("modernFunc:")
modernFunc()

print("main:")
var s = S(array: (.max, 1, 2, 3))
s.array.0 = 0
print_s(&s)

//--- src/ModernExec.swift

// RUN: %target-build-swift -emit-sil %t/src/ModernExec.swift -g -o %t/fragile/ModernExec.sil -I %t/include -I %t/fragile -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple -enable-experimental-feature ModernImportedCArrays
// RUN: %FileCheck --input-file %t/fragile/ModernExec.sil --check-prefix SIL %s
// RUN: %target-build-swift -emit-executable %t/src/ModernExec.swift %t/fragile/LegacyLib.o %t/fragile/ModernLib.o -g -o %t/fragile/ModernExec -I %t/include -I %t/fragile -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple -enable-experimental-feature ModernImportedCArrays
// RUN: %target-codesign %t/fragile/ModernExec
// RUN: %target-run %t/fragile/ModernExec > %t/fragile/ModernExec.txt
// RUN: %FileCheck --input-file %t/fragile/ModernExec.txt --check-prefix OUTPUT %s

// RUN: %target-build-swift -emit-sil %t/src/ModernExec.swift -g -o %t/resilient/ModernExec.sil -I %t/include -I %t/resilient -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple -enable-experimental-feature ModernImportedCArrays
// RUN: %FileCheck --input-file %t/resilient/ModernExec.sil --check-prefix SIL %s
// RUN: %target-build-swift -emit-executable %t/src/ModernExec.swift %t/resilient/LegacyLib.o %t/resilient/ModernLib.o -g -o %t/resilient/ModernExec -I %t/include -I %t/resilient -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple -enable-experimental-feature ModernImportedCArrays
// RUN: %target-codesign %t/resilient/ModernExec
// RUN: %target-run %t/resilient/ModernExec > %t/resilient/ModernExec.txt
// RUN: %FileCheck --input-file %t/resilient/ModernExec.txt --check-prefix OUTPUT %s

import CLib
import LegacyLib
import ModernLib

print("legacyFunc:")
legacyFunc()

print("modernFunc:")
modernFunc()

print("main:")
var s = S(array: [.max, 1, 2, 3])
s.array[0] = 0
print_s(&s)
