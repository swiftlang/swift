/*
Verify the debug info for the special stdlib builtin types
(IRGenModule::getOrCreateSpecialStlibBuiltinTypes()) carries everything a
debugger needs to build a builtin type descriptor out of DWARF: a mangled name
to find it by, a size, and an extra inhabitant count. A debugger has to do this
when there is no reflection metadata to read, which is always the case in
embedded Swift.

RUN: %target-swift-frontend -emit-ir -g -gdwarf-types %s -o - | %FileCheck %s
RUN: %target-swift-frontend -emit-ir -g               %s -o - | %FileCheck %s --check-prefix=ASTTYPES

The counts and sizes are target dependent (they follow LeastValidPointerValue,
which differs between Darwin and everything else, and between embedded and
non-embedded), so only their presence is checked here. Builtin.RawPointer is
the exception: it is nullable and null is its only extra inhabitant, so its
count is 1 everywhere.

CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "$sBoD",{{.*}} num_extra_inhabitants: {{[0-9]+}},
CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "$sBbD",{{.*}} num_extra_inhabitants: {{[0-9]+}},
CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "$sBpD",{{.*}} num_extra_inhabitants: 1,

A thin function type and an existential metatype are emitted as structs rather
than base types, so the count has to be attached to the struct.

CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "$syyXfD",{{.*}} num_extra_inhabitants: {{[0-9]+}},
CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "$sypXpD",{{.*}} num_extra_inhabitants: {{[0-9]+}},

Builtin.UnsafeValueBuffer has no dedicated case in createType() and must not
fall into the unhandled-type bucket, which would rename it "<unknown>".

CHECK-DAG: ![[BB:[0-9]+]] = !DIBasicType(name: "$sBBD", size: {{[0-9]+}})

AnyObject is not a builtin, but the reflection metadata emits it as the old
Builtin.UnknownObject for ABI compatibility, so a debugger asks for "BO".

CHECK-DAG: ![[BO:[0-9]+]] = !DIBasicType(name: "$sBOD", size: {{[0-9]+}}, num_extra_inhabitants: {{[0-9]+}})

Each one is anchored with a DW_TAG_imported_declaration; retaining the type is
not enough to survive dsymutil, since nothing in the program refers to these
types. (special-stdlib-builtin-types-dsym.swift verifies they survive.)

CHECK-DAG: !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !{{[0-9]+}}, entity: ![[BB]],
CHECK-DAG: !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !{{[0-9]+}}, entity: ![[BO]],

None of this is emitted below the DwarfTypes level, where there is no type
lowering information to describe anyway.

ASTTYPES-NOT: name: "$sBOD"
ASTTYPES-NOT: name: "$sBBD"
ASTTYPES-NOT: name: "$syyXfD"
*/

func use<T>(_ t: T) {}
use(42)
