// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(resilient_struct)) %S/../Inputs/resilient_struct.swift -enable-library-evolution -emit-module -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct
// RUN: %target-build-swift-dylib(%t/%target-library-name(resilient_enum)) %S/../Inputs/resilient_enum.swift -enable-library-evolution -emit-module -emit-module-path=%t/resilient_enum.swiftmodule -module-name=resilient_enum -I %t -L %t -lresilient_struct
// RUN: %target-swift-frontend -enable-type-layout -force-struct-type-layouts -primary-file %s -I %t -O -emit-ir | %FileCheck %s --check-prefix=CHECK

import resilient_struct
import resilient_enum

class SomeClass {}

// Start by checking that our layout strings are there and make sense

// RefStruct: Aligned group, 3 entries, pointer aligned pointers
// CHECK: [24 x i8] c"a\00\00\00\03{{2|3}}\00\00\00\01N{{2|3}}\00\00\00\01N{{2|3}}\00\00\00\01N

// AlignedStruct: Aligned group, 3 entries, byte aligned char, byte aligned short, pointer aligned pointer
// CHECK: [24 x i8] c"a\00\00\00\030\00\00\00\01c1\00\00\00\01s{{2|3}}\00\00\00\01N

// NestedStruct: Aligned group, 2 entries, byte aligned char, pointer aligned aligned group
// CHECK: [34 x i8] c"a\00\00\00\020\00\00\00\01c{{2|3}}\00\00\00\11a\00\00\00\020\00\00\00\01c{{2|3}}\00\00\00\01N

// FlattenedStruct: Aligned group, 3 entries, byte aligned char,byte aligned char pointer aligned pointer
// CHECK: [24 x i8] c"a\00\00\00\030\00\00\00\01c0\00\00\00\01c{{2|3}}\00\00\00\01N

// NoPayloadEnumStruct Aligned group with 3 entries: byte aligined char, and 2 ptr aligned ptrs
// CHECK: [24 x i8] c"a\00\00\00\030\00\00\00\01c{{2|3}}\00\00\00\01N{{2|3}}\00\00\00\01N

// SinglePayloadEnumStruct Aligned group with 2 entries: pointer aligned single payload enum and a ptr
// CHECK: [27 x i8] c"a\00\00\00\02{{2|3}}\00\00\00\0Ae\00\00\00\01\00\00\00\01N{{2|3}}\00\00\00\01N

// SingleEnumPayloadEnumStruct Aligned group with 2 entries: pointer aligned single payload enum enum and a ptr
// CHECK [36 x i8] c"a\00\00\00\02{{2|3}}\00\00\00\1{{2|3}}e\00\00\00\01\00\00\00\0Ae\00\00\00\01\00\00\00\01N{{2|3}}\00\00\00\01N

// MultiPayloadStruct Multipayload with 11 payload cases and 4 no payload cases
// CHECK [259 x i8] c"a\00\00\00\023\00\00\00\F2E\00\00\00\04\00\00\00\0B\00\00\00\01\00\00\00\11\00\00\00\11\00\00\00\11\00\00\00\11\00\00\00\11\00\00\00\11\00\00\00\11\00\00\00\11\00\00\00\17\00\00\00\1DNa\00\00\00\023\00\00\00\01N3\00\00\00\01Na\00\00\00\023\00\00\00\01N3\00\00\00\01Na\00\00\00\023\00\00\00\01N3\00\00\00\01Na\00\00\00\023\00\00\00\01N3\00\00\00\01Na\00\00\00\023\00\00\00\01N3\00\00\00\01Na\00\00\00\023\00\00\00\01N3\00\00\00\01Na\00\00\00\023\00\00\00\01N3\00\00\00\01Na\00\00\00\023\00\00\00\01N3\00\00\00\01Na\00\00\00\033\00\00\00\01N3\00\00\00\01N3\00\00\00\01Na\00\00\00\043\00\00\00\01N3\00\00\00\01N3\00\00\00\01N3\00\00\00\01L3\00\00\00\01N

// SingleArchetypeStruct Struct with a field of archetype index 1
// CHECK: [22 x i8] c"a\00\00\00\02?\00\00\00\05A\00\00\00\00{{2|3}}\00\00\00\01N

// MultiArchetypeStruct Struct with a field of archetype index 1
// CHECK: [32 x i8] c"a\00\00\00\03?\00\00\00\05A\00\00\00\00{{2|3}}\00\00\00\01N?\00\00\00\05A\00\00\00\01

// ResilientStruct Resilient types should encode their mangled name so we can look it up at runtime
// CHECK: [137 x i8] c"a\00\00\00\04?\00\00\00\05A\00\00\00\00{{2|3}}\00\00\00\11a\00\00\00\02{{2|3}}\00\00\00\01{{l|L}}{{2|3}}\00\00\00\01{{l|L}}?\00\00\00XR\00\00\00S14resilient_enum33ResilientSinglePayloadGenericEnumOy16bytecode_layouts9SomeClassCG{{2|3}}\00\00\00\01N

// Ensure that the value witness functions we generate for each type actually call into the generic destroy/initialize functions

// CHECK: define{{.*}} void @"$s16bytecode_layouts9RefStructVwxx"(
// CHECK:   swift_generic_destroy
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts9RefStructVwcp"(
// CHECK:   swift_generic_initialize
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts9RefStructVwca"(
// CHECK:   swift_generic_assign
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts9RefStructVwta"(
// CHECK:   swift_generic_assign
// CHECK: }
@_GenerateLayoutBytecode
struct RefStruct{
    let a: SomeClass
    let b: SomeClass
    let c: SomeClass
}

// CHECK: define{{.*}} void @"$s16bytecode_layouts13AlignedStructVwxx"(
// CHECK:   swift_generic_destroy
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts13AlignedStructVwcp"(
// CHECK:   swift_generic_initialize
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts13AlignedStructVwca"(
// CHECK:   swift_generic_assign
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts13AlignedStructVwta"(
// CHECK:   swift_generic_assign
// CHECK: }
@_GenerateLayoutBytecode
struct AlignedStruct {
    let a: UInt8
    let b: UInt16
    let c: SomeClass
}

// CHECK: define{{.*}} void @"$s16bytecode_layouts12NestedStructVwxx"(
// CHECK:   swift_generic_destroy
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts12NestedStructVwcp"(
// CHECK:   swift_generic_initialize
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts12NestedStructVwca"(
// CHECK:   swift_generic_assign
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts12NestedStructVwta"(
// CHECK:   swift_generic_assign
// CHECK: }
@_GenerateLayoutBytecode
struct NestedStruct {
    @_GenerateLayoutBytecode
    struct InnerStruct {
        let a: UInt8
        let b: SomeClass
    }

    let a: UInt8
    let b: InnerStruct
}


// CHECK: define{{.*}} void @"$s16bytecode_layouts15FlattenedStructVwxx"(
// CHECK:   swift_generic_destroy
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts15FlattenedStructVwcp"(
// CHECK:   swift_generic_initialize
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts15FlattenedStructVwca"(
// CHECK:   swift_generic_assign
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts15FlattenedStructVwta"(
// CHECK:   swift_generic_assign
// CHECK: }
@_GenerateLayoutBytecode
struct FlattenedStruct {
    let a: UInt8
    let b: UInt8
    let c: SomeClass
}

// CHECK: define{{.*}} void @"$s16bytecode_layouts19NoPayloadEnumStructVwxx"(
// CHECK:   swift_generic_destroy
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts19NoPayloadEnumStructVwcp"(
// CHECK:   swift_generic_initialize
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts19NoPayloadEnumStructVwca"(
// CHECK:   swift_generic_assign
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts19NoPayloadEnumStructVwta"(
// CHECK:   swift_generic_assign
// CHECK: }
@_GenerateLayoutBytecode
struct NoPayloadEnumStruct  {
    @_GenerateLayoutBytecode
    enum NoPayload  {
        case Only
        case NonPayload
        case Cases
    }

    let a: NoPayload
    let c: SomeClass
    let d: SomeClass
}

enum SinglePayloadEnum  {
    case Payload(c: SomeClass)
    case NoPayload
}

// CHECK: define{{.*}} void @"$s16bytecode_layouts23SinglePayloadEnumStructVwxx"(
// CHECK:   swift_generic_destroy
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts23SinglePayloadEnumStructVwcp"(
// CHECK:   swift_generic_initialize
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts23SinglePayloadEnumStructVwca"(
// CHECK:   swift_generic_assign
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts23SinglePayloadEnumStructVwta"(
// CHECK:   swift_generic_assign
// CHECK: }
@_GenerateLayoutBytecode
struct SinglePayloadEnumStruct  {
    let a: SinglePayloadEnum
    let c: SomeClass
}


// CHECK: define{{.*}} void @"$s16bytecode_layouts017SingleEnumPayloadD6StructVwxx"(
// CHECK:   swift_generic_destroy
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts017SingleEnumPayloadD6StructVwcp"(
// CHECK:   swift_generic_initialize
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts017SingleEnumPayloadD6StructVwca"(
// CHECK:   swift_generic_assign
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts017SingleEnumPayloadD6StructVwta"(
// CHECK:   swift_generic_assign
// CHECK: }
@_GenerateLayoutBytecode
struct SingleEnumPayloadEnumStruct  {
    @_GenerateLayoutBytecode
    enum EnumPayloadEnum {
        case EnumPayload(e: SinglePayloadEnum)
        case NoEnumPayload
    }
    let a: EnumPayloadEnum
    let c: SomeClass
}

// CHECK: define{{.*}} void @"$s16bytecode_layouts18MultiPayloadStructVwxx"(
// CHECK:   swift_generic_destroy
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts18MultiPayloadStructVwcp"(
// CHECK:   swift_generic_initialize
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts18MultiPayloadStructVwca"(
// CHECK:   swift_generic_assign
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts18MultiPayloadStructVwta"(
// CHECK:   swift_generic_assign
// CHECK: }
@_GenerateLayoutBytecode
struct MultiPayloadStruct  {
    @_GenerateLayoutBytecode
    enum MultiPayload  {
        case Payload1(c: SomeClass)
        case Payload2(c: SomeClass, d: SomeClass)
        case Payload3(c: SomeClass, d: SomeClass)
        case Payload4(c: SomeClass, d: SomeClass)
        case Payload5(c: SomeClass, d: SomeClass)
        case Payload6(c: SomeClass, d: SomeClass)
        case Payload7(c: SomeClass, d: SomeClass)
        case Payload8(c: SomeClass, d: SomeClass)
        case Payload9(c: SomeClass, d: SomeClass)
        case Payload10(c: SomeClass, d: SomeClass, e: SomeClass)
        case Payload11(c: SomeClass, d: SomeClass, e: SomeClass, f: UInt64)
        case NoPayload
        case NoPayload2
        case NoPayload3
        case NoPayload4
    }
    let a: MultiPayload
    let c: SomeClass
}


// CHECK: define{{.*}} void @"$s16bytecode_layouts21SingleArchetypeStructVwxx"(
// CHECK:   swift_generic_destroy
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts21SingleArchetypeStructVwcp"(
// CHECK:   swift_generic_initialize
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts21SingleArchetypeStructVwca"(
// CHECK:   swift_generic_assign
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts21SingleArchetypeStructVwta"(
// CHECK:   swift_generic_assign
// CHECK: }
@_GenerateLayoutBytecode
struct SingleArchetypeStruct<T>  {
    let a: T
    let b: SomeClass
}

// CHECK: define{{.*}} void @"$s16bytecode_layouts20MultiArchetypeStructVwxx"(
// CHECK:   swift_generic_destroy
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts20MultiArchetypeStructVwcp"(
// CHECK:   swift_generic_initialize
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts20MultiArchetypeStructVwca"(
// CHECK:   swift_generic_assign
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts20MultiArchetypeStructVwta"(
// CHECK:   swift_generic_assign
// CHECK: }
@_GenerateLayoutBytecode
struct MultiArchetypeStruct<S, T>  {
    let a: S
    let b: SomeClass
    let c: T
}

// CHECK: define{{.*}} void @"$s16bytecode_layouts15ResilientStructVwxx"(
// CHECK:   swift_generic_destroy
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts15ResilientStructVwcp"(
// CHECK:   swift_generic_initialize
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts15ResilientStructVwca"(
// CHECK:   swift_generic_assign
// CHECK: }

// CHECK: define{{.*}} %swift.opaque* @"$s16bytecode_layouts15ResilientStructVwta"(
// CHECK:   swift_generic_assign
// CHECK: }
@_GenerateLayoutBytecode
struct ResilientStruct<T> {
  init(a: T, b: Point, c: ResilientSinglePayloadGenericEnum<SomeClass>, d: SomeClass) {
    self.a = a
    self.b = b
    self.c = c
    self.d = d
  }
  let a: T
  let b: Point
  let c: ResilientSinglePayloadGenericEnum<SomeClass>
  let d: SomeClass
}
