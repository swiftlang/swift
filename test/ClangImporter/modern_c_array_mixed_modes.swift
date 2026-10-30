// Test that library code using the modern C array projection can be inlined
// into clients that use the legacy projection, and vice versa. Doing this
// requires that both projections be imported (even if one is hidden) and that
// XRef and SIL deserialization not mix them up.

// REQUIRES: swift_feature_ModernImportedCArrays
// REQUIRES: executable_test
// UNSUPPORTED: back_deployment_runtime || use_os_stdlib

// RUN: %empty-directory(%t/include)
// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/fragile)
// RUN: %empty-directory(%t/resilient)
// RUN: split-file %s %t

//
// Checks for SIL output:
// * legacy{,Newtype,Union}Func() and modern{,Newtype,Union}Func() are emitted
//   into clients and can coexist.
// * legacy{,Newtype,Union}Func() should always use the legacy projection.
// * modern{,Newtype,Union}Func() should always use the modern projection.
// * Key paths to the fields of S and NT are stored in both projections, but
//   key paths to a union's fields are always computed.
//

// SIL-LABEL: sil shared {{.*}}@$s9LegacyLib10legacyFuncyyF
// SIL-NOT: unchecked_trivial_bit_cast
// SIL: struct $S ({{%.*}})
// SIL: keypath $WritableKeyPath
// SIL-SAME: <S, (Int32, Int32, Int32, Int32)>, (root $S; stored_property #S.array : $(Int32, Int32, Int32, Int32))
// SIL: } // end sil function '$s9LegacyLib10legacyFuncyyF'

// SIL-LABEL: sil shared {{.*}}@$s9ModernLib10modernFuncyyF
// SIL: [[ALLOC_STACK_VALUE:%.*]] = alloc_stack $InlineArray<4, Int32>
// SIL: [[LOAD_VALUE:%.*]] = load [[ALLOC_STACK_VALUE]]
// SIL: [[BIT_CAST_VALUE:%.*]] = unchecked_trivial_bit_cast [[LOAD_VALUE]] to $(Int32, Int32, Int32, Int32)
// SIL: struct $S ({{%.*}})
// SIL: keypath $WritableKeyPath
// SIL-SAME: <S, InlineArray<4, Int32>>, (root $S; stored_property #S.array : $InlineArray<4, Int32>)
// SIL: } // end sil function '$s9ModernLib10modernFuncyyF'

// SIL-LABEL: sil shared {{.*}}@$s9LegacyLib15legacyUnionFuncyyF
// SIL: keypath $WritableKeyPath
// SIL-SAME: <U, (Int32, Int32, Int32, Int32)>, (root $U; settable_property $(Int32, Int32, Int32, Int32),
// SIL: } // end sil function '$s9LegacyLib15legacyUnionFuncyyF'

// SIL-LABEL: sil shared {{.*}}@$s9ModernLib15modernUnionFuncyyF
// SIL: keypath $WritableKeyPath
// SIL-SAME: <U, InlineArray<4, Int32>>, (root $U; settable_property $InlineArray<4, Int32>,
// SIL: } // end sil function '$s9ModernLib15modernUnionFuncyyF'

// SIL-LABEL: sil shared {{.*}}@$s9LegacyLib17legacyNewtypeFuncyyF
// SIL-NOT: unchecked_trivial_bit_cast
// SIL: keypath $KeyPath
// SIL-SAME: <NT, (Int32, Int32, Int32, Int32)>, (root $NT; stored_property #NT.rawValue : $(Int32, Int32, Int32, Int32))
// SIL: } // end sil function '$s9LegacyLib17legacyNewtypeFuncyyF'

// SIL-LABEL: sil shared {{.*}}@$s9ModernLib17modernNewtypeFuncyyF
// SIL: [[ALLOC_STACK_VALUE:%.*]] = alloc_stack $InlineArray<4, Int32>
// SIL: [[LOAD_VALUE:%.*]] = load [[ALLOC_STACK_VALUE]]
// SIL: keypath $KeyPath
// SIL-SAME: <NT, InlineArray<4, Int32>>, (root $NT; stored_property #NT.rawValue : $InlineArray<4, Int32>)
// SIL: } // end sil function '$s9ModernLib17modernNewtypeFuncyyF'

//
// Checks for execution output
//

// OUTPUT: legacyFunc:
// OUTPUT-NEXT: S 0 1 2 3
// OUTPUT-NEXT: modernFunc:
// OUTPUT-NEXT: S 0 1 2 3
// OUTPUT-NEXT: legacyUnionFunc:
// OUTPUT-NEXT: U 0 1 2 3
// OUTPUT-NEXT: modernUnionFunc:
// OUTPUT-NEXT: U 0 1 2 3
// OUTPUT-NEXT: legacyNewtypeFunc:
// OUTPUT-NEXT: NT 2147483647 1 2 3
// OUTPUT-NEXT: modernNewtypeFunc:
// OUTPUT-NEXT: NT 2147483647 1 2 3
// OUTPUT-NEXT: legacyTypedefFunc:
// OUTPUT-NEXT: Row 2147483647 1 2 3
// OUTPUT-NEXT: modernTypedefFunc:
// OUTPUT-NEXT: Row 2147483647 1 2 3
// OUTPUT-NEXT: legacyTypedefUnionFunc:
// OUTPUT-NEXT: Row 2147483647 1 2 3
// OUTPUT-NEXT: modernTypedefUnionFunc:
// OUTPUT-NEXT: Row 2147483647 1 2 3
// OUTPUT-NEXT: legacyTypedefBitfieldFunc:
// OUTPUT-NEXT: Row 2147483647 1 2 3
// OUTPUT-NEXT: modernTypedefBitfieldFunc:
// OUTPUT-NEXT: Row 2147483647 1 2 3
// OUTPUT-NEXT: legacyHugeFunc:
// OUTPUT-NEXT: Huge 0
// OUTPUT-NEXT: modernHugeFunc:
// OUTPUT-NEXT: Huge 7
// OUTPUT-NEXT: legacyNestedHugeFunc:
// OUTPUT-NEXT: NestedHuge 0
// OUTPUT-NEXT: modernNestedHugeFunc:
// OUTPUT-NEXT: NestedHuge 9
// OUTPUT-NEXT: modernHugeNewtypeFunc:
// OUTPUT-NEXT: HugeNT 7
// OUTPUT-NEXT: main:
// OUTPUT-NEXT: S 0 1 2 3
// OUTPUT-NEXT: U 0 1 2 3
// OUTPUT-NEXT: NT 2147483647 1 2 3
// OUTPUT-NEXT: Row 2147483647 1 2 3
// OUTPUT-NEXT: Row 2147483647 1 2 3
// OUTPUT-NEXT: Row 2147483647 1 2 3
// OUTPUT-NEXT: Huge 0
// OUTPUT-NEXT: NestedHuge 0

//--- include/CLib.h

struct S {
  // Stored property key path in both projections
  int array[4];
};

union U {
  // Computed property key path in both projections
  int array[4];
  float other;
};

typedef int NT[4] __attribute__((swift_newtype(struct)));

// Likewise, but too large to have a legacy projection.
typedef int HugeNT[5000] __attribute__((swift_newtype(struct)));

typedef int Row[4];

// Make sure we can deserialize references to `row` without confusing
// the projections for one another.
union RowUnion {
  Row row;
  int other;
};

struct BitfieldRow {
  unsigned flag : 1;
  Row row;
};

// Too large to have a legacy projection, so it can only be accessed in modern
// mode, but legacy mode still needs to be able to use the struct.
struct HugeRow {
  int values[5000];
};

// The same, but an array of huge arrays.
struct NestedHugeRow {
  int values[2][5000];
};

void print_s(const struct S *ptr);
void print_u(const union U *ptr);
void print_row(const Row *ptr);
void print_huge(const struct HugeRow *ptr);
void print_nested_huge(const struct NestedHugeRow *ptr);

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

@export(implementation) public func legacyUnionFunc() {
  var u = U(array: (.max, 1, 2, 3))
  u.array.0 = 0
  blackHole(\U.array)
  print_u(&u)
}

@export(implementation) public func legacyNewtypeFunc() {
  let nt = NT(rawValue: (.max, 1, 2, 3))
  blackHole(\NT.rawValue)
  print("NT", nt.rawValue.0, nt.rawValue.1, nt.rawValue.2, nt.rawValue.3)
}

@export(implementation) public func legacyTypedefFunc() {
  var row: Row = (.max, 1, 2, 3)
  print_row(&row)
}

@export(implementation) public func legacyTypedefUnionFunc() {
  var u = RowUnion()
  u.row = (.max, 1, 2, 3)
  print_row(&u.row)
}

@export(implementation) public func legacyTypedefBitfieldFunc() {
  var b = BitfieldRow(flag: 1, row: (.max, 1, 2, 3))
  print_row(&b.row)
}

@export(implementation) public func legacyHugeFunc() {
  var h = HugeRow()
  print_huge(&h)
}

@export(implementation) public func legacyNestedHugeFunc() {
  var h = NestedHugeRow()
  print_nested_huge(&h)
}

@inline(never) @usableFromInline internal func blackHole(_: some Any) {}

@c(print_s) @implementation public func print_s(_ ptr: UnsafePointer<S>!) {
  print("S", ptr.pointee.array.0, ptr.pointee.array.1, ptr.pointee.array.2, ptr.pointee.array.3)
}

@c(print_u) @implementation public func print_u(_ ptr: UnsafePointer<U>!) {
  print("U", ptr.pointee.array.0, ptr.pointee.array.1, ptr.pointee.array.2, ptr.pointee.array.3)
}

// FIXME: This can't be `@c @implementation public` until `@c` allows C arrays
@c(print_row) @usableFromInline internal func printRowImpl(_ ptr: UnsafePointer<Int32>!) {
  print("Row", ptr[0], ptr[1], ptr[2], ptr[3])
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

@export(implementation) public func modernUnionFunc() {
  var u = U(array: [.max, 1, 2, 3])
  u.array[0] = 0
  blackHole(\U.array)
  print_u(&u)
}

@export(implementation) public func modernNewtypeFunc() {
  let nt = NT(rawValue: [.max, 1, 2, 3])
  blackHole(\NT.rawValue)
  print("NT", nt.rawValue[0], nt.rawValue[1], nt.rawValue[2], nt.rawValue[3])
}

@export(implementation) public func modernTypedefFunc() {
  var row: Row = [.max, 1, 2, 3]
  print_row(&row)
}

@export(implementation) public func modernTypedefUnionFunc() {
  var u = RowUnion()
  u.row = [.max, 1, 2, 3]
  print_row(&u.row)
}

@export(implementation) public func modernTypedefBitfieldFunc() {
  var b = BitfieldRow(flag: 1, row: [.max, 1, 2, 3])
  print_row(&b.row)
}

@export(implementation) public func modernHugeFunc() {
  var h = HugeRow()
  h.values[4999] = 7
  print_huge(&h)
}

@export(implementation) public func modernHugeNewtypeFunc() {
  let nt = HugeNT(rawValue: InlineArray(repeating: 7))
  withUnsafeBytes(of: nt.rawValue) {
    print("HugeNT", $0.load(fromByteOffset: 4999 * MemoryLayout<Int32>.stride, as: Int32.self))
  }
}

@export(implementation) public func modernNestedHugeFunc() {
  var h = NestedHugeRow()
  h.values[1][4999] = 9
  print_nested_huge(&h)
}

// Reads through raw bytes to avoid InlineArray subscripts, which are badly
// broken in optimize_none_with_opaque_values (this is covered by other tests).
@c(print_huge) @implementation public func print_huge(_ ptr: UnsafePointer<HugeRow>!) {
  withUnsafeBytes(of: ptr.pointee.values) {
    print("Huge", $0.load(fromByteOffset: 4999 * MemoryLayout<Int32>.stride, as: Int32.self))
  }
}

@c(print_nested_huge) @implementation public func print_nested_huge(_ ptr: UnsafePointer<NestedHugeRow>!) {
  withUnsafeBytes(of: ptr.pointee.values) {
    print("NestedHuge", $0.load(fromByteOffset: (5000 + 4999) * MemoryLayout<Int32>.stride, as: Int32.self))
  }
}

@inline(never) @usableFromInline internal func blackHole(_: some Any) {}

//--- src/LegacyExec.swift

// RUN: %target-build-swift -emit-sil %t/src/LegacyExec.swift -g -o %t/fragile/LegacyExec.sil -I %t/include -I %t/fragile -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple -Onone
// RUN: %FileCheck --input-file %t/fragile/LegacyExec.sil --check-prefix SIL %s
// RUN: %target-build-swift -emit-executable %t/src/LegacyExec.swift %t/fragile/LegacyLib.o %t/fragile/ModernLib.o -g -o %t/fragile/LegacyExec -I %t/include -I %t/fragile -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple
// RUN: %target-codesign %t/fragile/LegacyExec
// RUN: %target-run %t/fragile/LegacyExec > %t/fragile/LegacyExec.txt
// RUN: %FileCheck --input-file %t/fragile/LegacyExec.txt --check-prefix OUTPUT %s

// RUN: %target-build-swift -emit-sil %t/src/LegacyExec.swift -g -o %t/resilient/LegacyExec.sil -I %t/include -I %t/resilient -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple -Onone
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

print("legacyUnionFunc:")
legacyUnionFunc()

print("modernUnionFunc:")
modernUnionFunc()

print("legacyNewtypeFunc:")
legacyNewtypeFunc()

print("modernNewtypeFunc:")
modernNewtypeFunc()

print("legacyTypedefFunc:")
legacyTypedefFunc()

print("modernTypedefFunc:")
modernTypedefFunc()

print("legacyTypedefUnionFunc:")
legacyTypedefUnionFunc()

print("modernTypedefUnionFunc:")
modernTypedefUnionFunc()

print("legacyTypedefBitfieldFunc:")
legacyTypedefBitfieldFunc()

print("modernTypedefBitfieldFunc:")
modernTypedefBitfieldFunc()

print("legacyHugeFunc:")
legacyHugeFunc()

print("modernHugeFunc:")
modernHugeFunc()

print("legacyNestedHugeFunc:")
legacyNestedHugeFunc()

print("modernNestedHugeFunc:")
modernNestedHugeFunc()

print("modernHugeNewtypeFunc:")
modernHugeNewtypeFunc()

print("main:")
var s = S(array: (.max, 1, 2, 3))
s.array.0 = 0
print_s(&s)

var un = U(array: (.max, 1, 2, 3))
un.array.0 = 0
print_u(&un)

let nt = NT(rawValue: (.max, 1, 2, 3))
print("NT", nt.rawValue.0, nt.rawValue.1, nt.rawValue.2, nt.rawValue.3)

var row: Row = (.max, 1, 2, 3)
print_row(&row)

var u = RowUnion()
u.row = (.max, 1, 2, 3)
print_row(&u.row)

var b = BitfieldRow(flag: 1, row: (.max, 1, 2, 3))
print_row(&b.row)

var h = HugeRow()
print_huge(&h)

var nh = NestedHugeRow()
print_nested_huge(&nh)

//--- src/ModernExec.swift

// RUN: %target-build-swift -emit-sil %t/src/ModernExec.swift -g -o %t/fragile/ModernExec.sil -I %t/include -I %t/fragile -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple -enable-experimental-feature ModernImportedCArrays -Onone
// RUN: %FileCheck --input-file %t/fragile/ModernExec.sil --check-prefix SIL %s
// RUN: %target-build-swift -emit-executable %t/src/ModernExec.swift %t/fragile/LegacyLib.o %t/fragile/ModernLib.o -g -o %t/fragile/ModernExec -I %t/include -I %t/fragile -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple -enable-experimental-feature ModernImportedCArrays
// RUN: %target-codesign %t/fragile/ModernExec
// RUN: %target-run %t/fragile/ModernExec > %t/fragile/ModernExec.txt
// RUN: %FileCheck --input-file %t/fragile/ModernExec.txt --check-prefix OUTPUT %s

// RUN: %target-build-swift -emit-sil %t/src/ModernExec.swift -g -o %t/resilient/ModernExec.sil -I %t/include -I %t/resilient -module-cache-path %t/module-cache -swift-version 5 -target %target-has-inline-array-triple -enable-experimental-feature ModernImportedCArrays -Onone
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

print("legacyUnionFunc:")
legacyUnionFunc()

print("modernUnionFunc:")
modernUnionFunc()

print("legacyNewtypeFunc:")
legacyNewtypeFunc()

print("modernNewtypeFunc:")
modernNewtypeFunc()

print("legacyTypedefFunc:")
legacyTypedefFunc()

print("modernTypedefFunc:")
modernTypedefFunc()

print("legacyTypedefUnionFunc:")
legacyTypedefUnionFunc()

print("modernTypedefUnionFunc:")
modernTypedefUnionFunc()

print("legacyTypedefBitfieldFunc:")
legacyTypedefBitfieldFunc()

print("modernTypedefBitfieldFunc:")
modernTypedefBitfieldFunc()

print("legacyHugeFunc:")
legacyHugeFunc()

print("modernHugeFunc:")
modernHugeFunc()

print("legacyNestedHugeFunc:")
legacyNestedHugeFunc()

print("modernNestedHugeFunc:")
modernNestedHugeFunc()

print("modernHugeNewtypeFunc:")
modernHugeNewtypeFunc()

print("main:")
var s = S(array: [.max, 1, 2, 3])
s.array[0] = 0
print_s(&s)

var un = U(array: [.max, 1, 2, 3])
un.array[0] = 0
print_u(&un)

let nt = NT(rawValue: [.max, 1, 2, 3])
print("NT", nt.rawValue[0], nt.rawValue[1], nt.rawValue[2], nt.rawValue[3])

var row: Row = [.max, 1, 2, 3]
print_row(&row)

var u = RowUnion()
u.row = [.max, 1, 2, 3]
print_row(&u.row)

var b = BitfieldRow(flag: 1, row: [.max, 1, 2, 3])
print_row(&b.row)

var h = HugeRow()
print_huge(&h)

var nh = NestedHugeRow()
print_nested_huge(&nh)
