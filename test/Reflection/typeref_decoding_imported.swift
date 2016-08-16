// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %S/Inputs/ImportedTypes.swift -parse-as-library -emit-module -emit-library -module-name TypesToReflect -o %t/libTypesToReflect.%target-dylib-extension -I %S/Inputs
// RUN: %target-swift-reflection-dump -binary-filename %t/libTypesToReflect.%target-dylib-extension | %FileCheck %s --check-prefix=CHECK-%target-ptrsize --check-prefix=CHECK-%target-cpu

// CHECK-32: FIELDS:
// CHECK-32: =======
// CHECK-32: TypesToReflect.HasCTypes
// CHECK-32: ------------------------
// CHECK-32: mcs: __C.MyCStruct
// CHECK-32: (struct __C.MyCStruct)

// CHECK-32: mce: __C.MyCEnum
// CHECK-32: (struct __C.MyCEnum)

// CHECK-32: mcu: __C.MyCUnion
// CHECK-32: (struct __C.MyCUnion)

// CHECK-32: mcsbf: __C.MyCStructWithBitfields
// CHECK-32: (struct __C.MyCStructWithBitfields)

// CHECK-32: ASSOCIATED TYPES:
// CHECK-32: =================

// CHECK-32: BUILTIN TYPES:
// CHECK-32: ==============

// CHECK-32: - __C.MyCStruct:
// CHECK-32: Size: 12
// CHECK-32: Alignment: 4
// CHECK-32: Stride: 12
// CHECK-32: NumExtraInhabitants: 0

// CHECK-32: - __C.MyCEnum:
// CHECK-32: Size: 4
// CHECK-32: Alignment: 4
// CHECK-32: Stride: 4
// CHECK-32: NumExtraInhabitants: 0

// CHECK-32: - __C.MyCUnion:
// CHECK-32: Size: 4
// CHECK-32: Alignment: 4
// CHECK-32: Stride: 4
// CHECK-32: NumExtraInhabitants: 0

// CHECK-i386: - __C.MyCStructWithBitfields:
// CHECK-i386: Size: 4
// CHECK-i386: Alignment: 4
// CHECK-i386: Stride: 4
// CHECK-i386: NumExtraInhabitants: 0

// CHECK-arm: - __C.MyCStructWithBitfields:
// CHECK-arm: Size: 2
// CHECK-arm: Alignment: 1
// CHECK-arm: Stride: 2
// CHECK-arm: NumExtraInhabitants: 0

// CHECK-32: CAPTURE DESCRIPTORS:
// CHECK-32: ====================


// CHECK-64: FIELDS:
// CHECK-64: =======
// CHECK-64: TypesToReflect.HasCTypes
// CHECK-64: ------------------------
// CHECK-64: mcs: __C.MyCStruct
// CHECK-64: (struct __C.MyCStruct)

// CHECK-64: mce: __C.MyCEnum
// CHECK-64: (struct __C.MyCEnum)

// CHECK-64: mcu: __C.MyCUnion
// CHECK-64: (struct __C.MyCUnion)

// CHECK-64: mcsbf: __C.MyCStructWithBitfields
// CHECK-64: (struct __C.MyCStructWithBitfields)

// CHECK-64: ASSOCIATED TYPES:
// CHECK-64: =================

// CHECK-64: BUILTIN TYPES:
// CHECK-64: ==============

// CHECK-64: - __C.MyCStruct:
// CHECK-64: Size: 24
// CHECK-64: Alignment: 8
// CHECK-64: Stride: 24
// CHECK-64: NumExtraInhabitants: 0

// CHECK-64: - __C.MyCEnum:
// CHECK-64: Size: 4
// CHECK-64: Alignment: 4
// CHECK-64: Stride: 4
// CHECK-64: NumExtraInhabitants: 0

// CHECK-64: - __C.MyCUnion:
// CHECK-64: Size: 8
// CHECK-64: Alignment: 8
// CHECK-64: Stride: 8
// CHECK-64: NumExtraInhabitants: 0

// CHECK-64: - __C.MyCStructWithBitfields:
// CHECK-64: Size: 4
// CHECK-64: Alignment: 4
// CHECK-64: Stride: 4
// CHECK-64: NumExtraInhabitants: 0

// CHECK-64: CAPTURE DESCRIPTORS:
// CHECK-64: ====================

