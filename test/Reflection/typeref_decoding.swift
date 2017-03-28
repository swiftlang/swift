// REQUIRES: no_asan
// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %S/Inputs/ConcreteTypes.swift %S/Inputs/GenericTypes.swift %S/Inputs/Protocols.swift %S/Inputs/Extensions.swift %S/Inputs/Closures.swift -parse-as-library -emit-module -emit-library -module-name TypesToReflect -o %t/libTypesToReflect.%target-dylib-extension
// RUN: %target-swift-reflection-dump -binary-filename %t/libTypesToReflect.%target-dylib-extension | %FileCheck %s

// CHECK: FIELDS:
// CHECK: =======
// CHECK: TypesToReflect.Box
// CHECK: ------------------
// CHECK: item: A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: TypesToReflect.C
// CHECK: ----------------
// CHECK: aClass: TypesToReflect.C
// CHECK: (class TypesToReflect.C)

// CHECK: aStruct: TypesToReflect.S
// CHECK: (struct TypesToReflect.S)

// CHECK: anEnum: TypesToReflect.E
// CHECK: (enum TypesToReflect.E)

// CHECK: aTuple: (TypesToReflect.C, TypesToReflect.S, TypesToReflect.E, Swift.Int)
// CHECK: (tuple
// CHECK:   (class TypesToReflect.C)
// CHECK:   (struct TypesToReflect.S)
// CHECK:   (enum TypesToReflect.E)
// CHECK:   (struct Swift.Int))

// CHECK: aTupleWithLabels: (a : TypesToReflect.C, s : TypesToReflect.S, e : TypesToReflect.E)
// CHECK: (tuple
// CHECK:   (class TypesToReflect.C)
// CHECK:   (struct TypesToReflect.S)
// CHECK:   (enum TypesToReflect.E))

// CHECK: aMetatype: TypesToReflect.C.Type
// CHECK: (metatype
// CHECK:   (class TypesToReflect.C))

// CHECK: aFunction: (TypesToReflect.C, TypesToReflect.S, TypesToReflect.E, Swift.Int) -> Swift.Int
// CHECK: (function
// CHECK:   (class TypesToReflect.C)
// CHECK:   (struct TypesToReflect.S)
// CHECK:   (enum TypesToReflect.E)
// CHECK:   (struct Swift.Int)
// CHECK:   (struct Swift.Int))

// CHECK: aFunctionWithVarArgs: (TypesToReflect.C, TypesToReflect.S...) -> ()
// CHECK: (function
// CHECK:   (class TypesToReflect.C)
// CHECK:   (bound_generic_struct Swift.Array
// CHECK:     (struct TypesToReflect.S))
// CHECK:   (tuple))

// CHECK: TypesToReflect.S.NestedS
// CHECK: ------------------------
// CHECK: aField: Swift.Int
// CHECK: (struct Swift.Int)

// CHECK: TypesToReflect.S
// CHECK: ----------------
// CHECK: aClass: TypesToReflect.C
// CHECK: (class TypesToReflect.C)

// CHECK: aStruct: TypesToReflect.Box<TypesToReflect.S>
// CHECK: (bound_generic_class TypesToReflect.Box
// CHECK:   (struct TypesToReflect.S))

// CHECK: anEnum: TypesToReflect.Box<TypesToReflect.E>
// CHECK: (bound_generic_class TypesToReflect.Box
// CHECK:   (enum TypesToReflect.E))

// CHECK: aTuple: (TypesToReflect.C, TypesToReflect.Box<TypesToReflect.S>, TypesToReflect.Box<TypesToReflect.E>, Swift.Int)
// CHECK: (tuple
// CHECK:   (class TypesToReflect.C)
// CHECK:   (bound_generic_class TypesToReflect.Box
// CHECK:     (struct TypesToReflect.S))
// CHECK:   (bound_generic_class TypesToReflect.Box
// CHECK:     (enum TypesToReflect.E))
// CHECK:   (struct Swift.Int))

// CHECK: aMetatype: TypesToReflect.C.Type
// CHECK: (metatype
// CHECK:   (class TypesToReflect.C))

// CHECK: aFunction: (TypesToReflect.C, TypesToReflect.S, TypesToReflect.E, Swift.Int) -> Swift.Int
// CHECK: (function
// CHECK:   (class TypesToReflect.C)
// CHECK:   (struct TypesToReflect.S)
// CHECK:   (enum TypesToReflect.E)
// CHECK:   (struct Swift.Int)
// CHECK:   (struct Swift.Int))

// CHECK: aFunctionWithThinRepresentation: @convention(thin) () -> ()
// CHECK: (function convention=thin
// CHECK:   (tuple))

// CHECK: aFunctionWithCRepresentation: @convention(c) () -> ()
// CHECK: (function convention=c
// CHECK:   (tuple))

// CHECK: TypesToReflect.E
// CHECK: ----------------
// CHECK: Class: TypesToReflect.C
// CHECK: (class TypesToReflect.C)

// CHECK: Struct: TypesToReflect.S
// CHECK: (struct TypesToReflect.S)

// CHECK: Enum: TypesToReflect.E
// CHECK: (enum TypesToReflect.E)

// CHECK: Function: (TypesToReflect.C, TypesToReflect.S, TypesToReflect.E, Swift.Int) -> ()
// CHECK: (function
// CHECK:   (class TypesToReflect.C)
// CHECK:   (struct TypesToReflect.S)
// CHECK:   (enum TypesToReflect.E)
// CHECK:   (struct Swift.Int)
// CHECK:   (tuple))

// CHECK: Tuple: (TypesToReflect.C, TypesToReflect.S, Swift.Int)
// CHECK: (tuple
// CHECK:   (class TypesToReflect.C)
// CHECK:   (struct TypesToReflect.S)
// CHECK:   (struct Swift.Int))

// CHECK: IndirectTuple: (TypesToReflect.C, TypesToReflect.S, TypesToReflect.E, Swift.Int)
// CHECK: (tuple
// CHECK:   (class TypesToReflect.C)
// CHECK:   (struct TypesToReflect.S)
// CHECK:   (enum TypesToReflect.E)
// CHECK:   (struct Swift.Int))

// CHECK: NestedStruct: TypesToReflect.S.NestedS
// CHECK: (struct TypesToReflect.S.NestedS
// CHECK:   (struct TypesToReflect.S))

// CHECK: Metatype

// CHECK: EmptyCase

// CHECK: TypesToReflect.References
// CHECK: -------------------------
// CHECK: strongRef: TypesToReflect.C
// CHECK: (class TypesToReflect.C)

// CHECK: weakRef: weak Swift.Optional<TypesToReflect.C>
// CHECK: (weak_storage
// CHECK:   (bound_generic_enum Swift.Optional
// CHECK:     (class TypesToReflect.C)))

// CHECK: unownedRef: unowned TypesToReflect.C
// CHECK: (unowned_storage
// CHECK:   (class TypesToReflect.C))

// CHECK: unownedUnsafeRef: unowned(unsafe) TypesToReflect.C
// CHECK: (unmanaged_storage
// CHECK:   (class TypesToReflect.C))

// CHECK: TypesToReflect.C1
// CHECK: -----------------
// CHECK: aClass: TypesToReflect.C1<A>
// CHECK: (bound_generic_class TypesToReflect.C1
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: aStruct: TypesToReflect.S1<A>
// CHECK: (bound_generic_struct TypesToReflect.S1
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: anEnum: TypesToReflect.E1<A>
// CHECK: (bound_generic_enum TypesToReflect.E1
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: function: (TypesToReflect.C1<A>) -> (TypesToReflect.S1<A>) -> (TypesToReflect.E1<A>) -> Swift.Int
// CHECK: (function
// CHECK:   (bound_generic_class TypesToReflect.C1
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (function
// CHECK:     (bound_generic_struct TypesToReflect.S1
// CHECK:       (generic_type_parameter depth=0 index=0))
// CHECK:     (function
// CHECK:       (bound_generic_enum TypesToReflect.E1
// CHECK:         (generic_type_parameter depth=0 index=0))
// CHECK:       (struct Swift.Int))))

// CHECK: tuple: (TypesToReflect.C1<A>, TypesToReflect.S1<A>, TypesToReflect.E1<A>, Swift.Int)
// CHECK: (tuple
// CHECK:   (bound_generic_class TypesToReflect.C1
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (bound_generic_struct TypesToReflect.S1
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (bound_generic_enum TypesToReflect.E1
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (struct Swift.Int))

// CHECK: dependentMember: A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: TypesToReflect.C2
// CHECK: -----------------
// CHECK: aClass: TypesToReflect.C1<A>
// CHECK: (bound_generic_class TypesToReflect.C1
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: aStruct: TypesToReflect.S1<A>
// CHECK: (bound_generic_struct TypesToReflect.S1
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: anEnum: TypesToReflect.E1<A>
// CHECK: (bound_generic_enum TypesToReflect.E1
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: function: (TypesToReflect.C1<A>) -> (TypesToReflect.S1<A>) -> (TypesToReflect.E1<A>) -> Swift.Int
// CHECK: (function
// CHECK:   (bound_generic_class TypesToReflect.C1
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (function
// CHECK:     (bound_generic_struct TypesToReflect.S1
// CHECK:       (generic_type_parameter depth=0 index=0))
// CHECK:     (function
// CHECK:       (bound_generic_enum TypesToReflect.E1
// CHECK:         (generic_type_parameter depth=0 index=0))
// CHECK:       (struct Swift.Int))))

// CHECK: tuple: (TypesToReflect.C2<A>, TypesToReflect.S2<A>, TypesToReflect.E2<A>, Swift.Int)
// CHECK: (tuple
// CHECK:   (bound_generic_class TypesToReflect.C2
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (bound_generic_struct TypesToReflect.S2
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (bound_generic_enum TypesToReflect.E2
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (struct Swift.Int))

// CHECK: primaryArchetype: A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: dependentMember1: A.Inner
// CHECK: (dependent_member
// CHECK:   (protocol TypesToReflect.P1)
// CHECK:   (generic_type_parameter depth=0 index=0) member=Inner)

// CHECK: TypesToReflect.C3
// CHECK: -----------------
// CHECK: aClass: TypesToReflect.C3<A>
// CHECK: (bound_generic_class TypesToReflect.C3
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: aStruct: TypesToReflect.S3<A>
// CHECK: (bound_generic_struct TypesToReflect.S3
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: anEnum: TypesToReflect.E3<A>
// CHECK: (bound_generic_enum TypesToReflect.E3
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: function: (TypesToReflect.C3<A>) -> (TypesToReflect.S3<A>) -> (TypesToReflect.E3<A>) -> Swift.Int
// CHECK: (function
// CHECK:   (bound_generic_class TypesToReflect.C3
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (function
// CHECK:     (bound_generic_struct TypesToReflect.S3
// CHECK:       (generic_type_parameter depth=0 index=0))
// CHECK:     (function
// CHECK:       (bound_generic_enum TypesToReflect.E3
// CHECK:         (generic_type_parameter depth=0 index=0))
// CHECK:       (struct Swift.Int))))

// CHECK: tuple: (TypesToReflect.C3<A>, TypesToReflect.S3<A>, TypesToReflect.E3<A>, Swift.Int)
// CHECK: (tuple
// CHECK:   (bound_generic_class TypesToReflect.C3
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (bound_generic_struct TypesToReflect.S3
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (bound_generic_enum TypesToReflect.E3
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (struct Swift.Int))

// CHECK: primaryArchetype: A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: dependentMember1: A.Outer
// CHECK: (dependent_member
// CHECK:   (protocol TypesToReflect.P2)
// CHECK:   (generic_type_parameter depth=0 index=0) member=Outer)

// CHECK: dependentMember2: A.Outer.Inner
// CHECK: (dependent_member
// CHECK:   (protocol TypesToReflect.P1)
// CHECK:   (dependent_member
// CHECK:     (protocol TypesToReflect.P2)
// CHECK:     (generic_type_parameter depth=0 index=0) member=Outer) member=Inner)

// CHECK: TypesToReflect.C4
// CHECK: -----------------
// CHECK: TypesToReflect.S1
// CHECK: -----------------
// CHECK: aClass: TypesToReflect.C1<A>
// CHECK: (bound_generic_class TypesToReflect.C1
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: aStruct: TypesToReflect.Box<TypesToReflect.S1<A>>
// CHECK: (bound_generic_class TypesToReflect.Box
// CHECK:   (bound_generic_struct TypesToReflect.S1
// CHECK:     (generic_type_parameter depth=0 index=0)))

// CHECK: anEnum: TypesToReflect.Box<TypesToReflect.E1<A>>
// CHECK: (bound_generic_class TypesToReflect.Box
// CHECK:   (bound_generic_enum TypesToReflect.E1
// CHECK:     (generic_type_parameter depth=0 index=0)))

// CHECK: function: (TypesToReflect.C1<A>) -> (TypesToReflect.S1<A>) -> (TypesToReflect.E1<A>) -> Swift.Int
// CHECK: (function
// CHECK:   (bound_generic_class TypesToReflect.C1
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (function
// CHECK:     (bound_generic_struct TypesToReflect.S1
// CHECK:       (generic_type_parameter depth=0 index=0))
// CHECK:     (function
// CHECK:       (bound_generic_enum TypesToReflect.E1
// CHECK:         (generic_type_parameter depth=0 index=0))
// CHECK:       (struct Swift.Int))))

// CHECK: tuple: (TypesToReflect.C1<A>, TypesToReflect.Box<TypesToReflect.S1<A>>, TypesToReflect.Box<TypesToReflect.E1<A>>, Swift.Int)
// CHECK: (tuple
// CHECK:   (bound_generic_class TypesToReflect.C1
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (bound_generic_class TypesToReflect.Box
// CHECK:     (bound_generic_struct TypesToReflect.S1
// CHECK:       (generic_type_parameter depth=0 index=0)))
// CHECK:   (bound_generic_class TypesToReflect.Box
// CHECK:     (bound_generic_enum TypesToReflect.E1
// CHECK:       (generic_type_parameter depth=0 index=0)))
// CHECK:   (struct Swift.Int))

// CHECK: primaryArchetype: A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: TypesToReflect.S2
// CHECK: -----------------
// CHECK: aClass: TypesToReflect.C2<A>
// CHECK: (bound_generic_class TypesToReflect.C2
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: aStruct: TypesToReflect.Box<TypesToReflect.S2<A>>
// CHECK: (bound_generic_class TypesToReflect.Box
// CHECK:   (bound_generic_struct TypesToReflect.S2
// CHECK:     (generic_type_parameter depth=0 index=0)))

// CHECK: anEnum: TypesToReflect.Box<TypesToReflect.E2<A>>
// CHECK: (bound_generic_class TypesToReflect.Box
// CHECK:   (bound_generic_enum TypesToReflect.E2
// CHECK:     (generic_type_parameter depth=0 index=0)))

// CHECK: function: (TypesToReflect.C2<A>) -> (TypesToReflect.S2<A>) -> (TypesToReflect.E2<A>) -> Swift.Int
// CHECK: (function
// CHECK:   (bound_generic_class TypesToReflect.C2
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (function
// CHECK:     (bound_generic_struct TypesToReflect.S2
// CHECK:       (generic_type_parameter depth=0 index=0))
// CHECK:     (function
// CHECK:       (bound_generic_enum TypesToReflect.E2
// CHECK:         (generic_type_parameter depth=0 index=0))
// CHECK:       (struct Swift.Int))))

// CHECK: tuple: (TypesToReflect.C2<A>, TypesToReflect.Box<TypesToReflect.S2<A>>, TypesToReflect.Box<TypesToReflect.E2<A>>, Swift.Int)
// CHECK: (tuple
// CHECK:   (bound_generic_class TypesToReflect.C2
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (bound_generic_class TypesToReflect.Box
// CHECK:     (bound_generic_struct TypesToReflect.S2
// CHECK:       (generic_type_parameter depth=0 index=0)))
// CHECK:   (bound_generic_class TypesToReflect.Box
// CHECK:     (bound_generic_enum TypesToReflect.E2
// CHECK:       (generic_type_parameter depth=0 index=0)))
// CHECK:   (struct Swift.Int))

// CHECK: primaryArchetype: A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: dependentMember1: A.Inner
// CHECK: (dependent_member
// CHECK:   (protocol TypesToReflect.P1)
// CHECK:   (generic_type_parameter depth=0 index=0) member=Inner)

// CHECK: TypesToReflect.S3
// CHECK: -----------------
// CHECK: aClass: TypesToReflect.C3<A>
// CHECK: (bound_generic_class TypesToReflect.C3
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: aStruct: TypesToReflect.Box<TypesToReflect.S3<A>>
// CHECK: (bound_generic_class TypesToReflect.Box
// CHECK:   (bound_generic_struct TypesToReflect.S3
// CHECK:     (generic_type_parameter depth=0 index=0)))

// CHECK: anEnum: TypesToReflect.Box<TypesToReflect.E3<A>>
// CHECK: (bound_generic_class TypesToReflect.Box
// CHECK:   (bound_generic_enum TypesToReflect.E3
// CHECK:     (generic_type_parameter depth=0 index=0)))

// CHECK: function: (TypesToReflect.C3<A>) -> (TypesToReflect.S3<A>) -> (TypesToReflect.E3<A>) -> Swift.Int
// CHECK: (function
// CHECK:   (bound_generic_class TypesToReflect.C3
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (function
// CHECK:     (bound_generic_struct TypesToReflect.S3
// CHECK:       (generic_type_parameter depth=0 index=0))
// CHECK:     (function
// CHECK:       (bound_generic_enum TypesToReflect.E3
// CHECK:         (generic_type_parameter depth=0 index=0))
// CHECK:       (struct Swift.Int))))

// CHECK: tuple: (TypesToReflect.C3<A>, TypesToReflect.Box<TypesToReflect.S3<A>>, TypesToReflect.Box<TypesToReflect.E3<A>>, Swift.Int)
// CHECK: (tuple
// CHECK:   (bound_generic_class TypesToReflect.C3
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (bound_generic_class TypesToReflect.Box
// CHECK:     (bound_generic_struct TypesToReflect.S3
// CHECK:       (generic_type_parameter depth=0 index=0)))
// CHECK:   (bound_generic_class TypesToReflect.Box
// CHECK:     (bound_generic_enum TypesToReflect.E3
// CHECK:       (generic_type_parameter depth=0 index=0)))
// CHECK:   (struct Swift.Int))

// CHECK: primaryArchetype: A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: dependentMember1: A.Outer
// CHECK: (dependent_member
// CHECK:   (protocol TypesToReflect.P2)
// CHECK:   (generic_type_parameter depth=0 index=0) member=Outer)

// CHECK: dependentMember2: A.Outer.Inner
// CHECK: (dependent_member
// CHECK:   (protocol TypesToReflect.P1)
// CHECK:   (dependent_member
// CHECK:     (protocol TypesToReflect.P2)
// CHECK:     (generic_type_parameter depth=0 index=0) member=Outer) member=Inner)

// CHECK: TypesToReflect.S4
// CHECK: -----------------
// CHECK: TypesToReflect.E1
// CHECK: -----------------
// CHECK: Class: TypesToReflect.C1<A>
// CHECK: (bound_generic_class TypesToReflect.C1
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: Struct: TypesToReflect.S1<A>
// CHECK: (bound_generic_struct TypesToReflect.S1
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: Enum: TypesToReflect.E1<A>
// CHECK: (bound_generic_enum TypesToReflect.E1
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: Int: Swift.Int
// CHECK: (struct Swift.Int)

// CHECK: Function: (A) -> TypesToReflect.E1<A>
// CHECK: (function
// CHECK:   (generic_type_parameter depth=0 index=0)
// CHECK:   (bound_generic_enum TypesToReflect.E1
// CHECK:     (generic_type_parameter depth=0 index=0)))

// CHECK: Tuple: (TypesToReflect.C1<A>, TypesToReflect.S1<A>, Swift.Int)
// CHECK: (tuple
// CHECK:   (bound_generic_class TypesToReflect.C1
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (bound_generic_struct TypesToReflect.S1
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (struct Swift.Int))

// CHECK: Primary: A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: Metatype: A.Type
// CHECK: (metatype
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: TypesToReflect.E2
// CHECK: -----------------
// CHECK: Class: TypesToReflect.C2<A>
// CHECK: (bound_generic_class TypesToReflect.C2
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: Struct: TypesToReflect.S2<A>
// CHECK: (bound_generic_struct TypesToReflect.S2
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: Enum: TypesToReflect.E2<A>
// CHECK: (bound_generic_enum TypesToReflect.E2
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: Function: (A.Type) -> TypesToReflect.E1<A>
// CHECK: (function
// CHECK:   (metatype
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (bound_generic_enum TypesToReflect.E1
// CHECK:     (generic_type_parameter depth=0 index=0)))

// CHECK: Tuple: (TypesToReflect.C2<A>, TypesToReflect.S2<A>, Swift.Int)
// CHECK: (tuple
// CHECK:   (bound_generic_class TypesToReflect.C2
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (bound_generic_struct TypesToReflect.S2
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (struct Swift.Int))

// CHECK: Primary: A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: DependentMemberInner: A.Inner
// CHECK: (dependent_member
// CHECK:   (protocol TypesToReflect.P1)
// CHECK:   (generic_type_parameter depth=0 index=0) member=Inner)

// CHECK: ExistentialMetatype: A.Type
// CHECK: (metatype
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: TypesToReflect.E3
// CHECK: -----------------
// CHECK: Class: TypesToReflect.C3<A>
// CHECK: (bound_generic_class TypesToReflect.C3
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: Struct: TypesToReflect.S3<A>
// CHECK: (bound_generic_struct TypesToReflect.S3
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: Enum: TypesToReflect.E3<A>
// CHECK: (bound_generic_enum TypesToReflect.E3
// CHECK:   (generic_type_parameter depth=0 index=0))

// CHECK: Function: (A.Type.Type) -> TypesToReflect.E1<A>
// CHECK: (function
// CHECK:   (metatype
// CHECK:     (metatype
// CHECK:       (generic_type_parameter depth=0 index=0)))
// CHECK:   (bound_generic_enum TypesToReflect.E1
// CHECK:     (generic_type_parameter depth=0 index=0)))

// CHECK: Tuple: (TypesToReflect.C3<A>, TypesToReflect.S3<A>, Swift.Int)
// CHECK: (tuple
// CHECK:   (bound_generic_class TypesToReflect.C3
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (bound_generic_struct TypesToReflect.S3
// CHECK:     (generic_type_parameter depth=0 index=0))
// CHECK:   (struct Swift.Int))

// CHECK: Primary: A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: DependentMemberOuter: A.Outer
// CHECK: (dependent_member
// CHECK:   (protocol TypesToReflect.P2)
// CHECK:   (generic_type_parameter depth=0 index=0) member=Outer)

// CHECK: DependentMemberInner: A.Outer.Inner
// CHECK: (dependent_member
// CHECK:   (protocol TypesToReflect.P1)
// CHECK:   (dependent_member
// CHECK:     (protocol TypesToReflect.P2)
// CHECK:     (generic_type_parameter depth=0 index=0) member=Outer) member=Inner)

// CHECK: TypesToReflect.E4
// CHECK: -----------------
// CHECK: TypesToReflect.P1
// CHECK: -----------------
// CHECK: TypesToReflect.P2
// CHECK: -----------------
// CHECK: TypesToReflect.P3
// CHECK: -----------------
// CHECK: TypesToReflect.P4
// CHECK: -----------------
// CHECK: TypesToReflect.ClassBoundP
// CHECK: --------------------------

// CHECK: ASSOCIATED TYPES:
// CHECK: =================
// CHECK: - TypesToReflect.C1 : TypesToReflect.ClassBoundP
// CHECK: typealias Inner = A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: - TypesToReflect.C4 : TypesToReflect.P1
// CHECK: typealias Inner = A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: - TypesToReflect.C4 : TypesToReflect.P2
// CHECK: typealias Outer = A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: - TypesToReflect.S4 : TypesToReflect.P1
// CHECK: typealias Inner = A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: - TypesToReflect.S4 : TypesToReflect.P2
// CHECK: typealias Outer = A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: - TypesToReflect.E4 : TypesToReflect.P1
// CHECK: typealias Inner = A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: - TypesToReflect.E4 : TypesToReflect.P2
// CHECK: typealias Outer = B
// CHECK: (generic_type_parameter depth=0 index=1)

// CHECK: - TypesToReflect.E4 : TypesToReflect.P3
// CHECK: typealias First = A
// CHECK: (generic_type_parameter depth=0 index=0)

// CHECK: typealias Second = B
// CHECK: (generic_type_parameter depth=0 index=1)

// CHECK: - TypesToReflect.S : TypesToReflect.P4
// CHECK: typealias Result = Swift.Int
// CHECK: (struct Swift.Int)

// CHECK: BUILTIN TYPES:
// CHECK: ==============

// CHECK: CAPTURE DESCRIPTORS:
// CHECK: ====================
// CHECK: - Capture types:
// CHECK: (sil_box
// CHECK:   (generic_type_parameter depth=0 index=0))
// CHECK: - Metadata sources:
// CHECK: (generic_type_parameter depth=0 index=0)
// CHECK: (closure_binding index=0)
// CHECK: (generic_type_parameter depth=0 index=1)
// CHECK: (closure_binding index=1)

// CHECK: - Capture types:
// CHECK: (struct Swift.Int)
// CHECK: - Metadata sources:

// CHECK: - Capture types:
// CHECK: (bound_generic_class TypesToReflect.C1
// CHECK:   (generic_type_parameter depth=0 index=1))
// CHECK: - Metadata sources:
// CHECK: (generic_type_parameter depth=0 index=0)
// CHECK: (closure_binding index=0)
// CHECK: (generic_type_parameter depth=0 index=1)
// CHECK: (generic_argument index=0
// CHECK:   (reference_capture index=0))

