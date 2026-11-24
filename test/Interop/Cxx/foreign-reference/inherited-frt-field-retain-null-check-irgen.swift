// The synthesized getter for a foreign reference pointer field inherited from
// a C++ base class null-checks the field before retaining it, unless the field
// is _Nonnull or the retain function accepts a _Nullable pointer.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-emit-ir %t/test.swift -I %t/Inputs -cxx-interoperability-mode=default -Onone -o %t/test.ll
// RUN: %FileCheck %s --check-prefix=UNSPECIFIED < %t/test.ll
// RUN: %FileCheck %s --check-prefix=NULLABLE < %t/test.ll
// RUN: %FileCheck %s --check-prefix=NONNULL < %t/test.ll
// RUN: %FileCheck %s --check-prefix=NULLABLE-RETAIN < %t/test.ll
// RUN: %FileCheck %s --check-prefix=NULLABLE-RETAIN-DEF < %t/test.ll

//--- Inputs/module.modulemap
module M {
  header "m.h"
  requires cplusplus
}

//--- Inputs/m.h
#include <swift/bridging>

struct SWIFT_SHARED_REFERENCE(retainA, releaseA) A { int refCount = 1; };
inline void retainA(A *p) { p->refCount++; }
inline void releaseA(A *p) { p->refCount--; }

// Retain accepts null.
struct SWIFT_SHARED_REFERENCE(retainB, releaseB) B { int refCount = 1; };
inline void retainB(B *_Nullable p) { if (p) p->refCount++; }
inline void releaseB(B *_Nullable p) { if (p) p->refCount--; }

// Retain accepts null, but only its definition says so.
struct C;
inline void retainC(C *);
inline void releaseC(C *);
struct SWIFT_SHARED_REFERENCE(retainC, releaseC) C { int refCount = 1; };
inline void retainC(C *_Nullable p) { if (p) p->refCount++; }
inline void releaseC(C *_Nullable p) { if (p) p->refCount--; }

struct Base {
  A *unspecified = nullptr;
  A *_Nullable nullable = nullptr;
  A *_Nonnull nonnull;
  B *nullableRetain = nullptr;
  C *nullableRetainDefinition = nullptr;
};
struct Derived : Base {};

//--- test.swift
import M

@available(SwiftStdlib 5.8, *)
public func unspecified(_ d: Derived) -> A? { d.unspecified }
@available(SwiftStdlib 5.8, *)
public func nullable(_ d: Derived) -> A? { d.nullable }
@available(SwiftStdlib 5.8, *)
public func nonnull(_ d: Derived) -> A { d.nonnull }
@available(SwiftStdlib 5.8, *)
public func nullableRetain(_ d: Derived) -> B? { d.nullableRetain }
@available(SwiftStdlib 5.8, *)
public func nullableRetainDefinition(_ d: Derived) -> C? { d.nullableRetainDefinition }

// UNSPECIFIED-LABEL: define {{.*}} @{{.*}}__synthesizedBaseGetterAccessor_unspecified{{(Ev|@)}}
// UNSPECIFIED-NOT: {{^}}}
// UNSPECIFIED: icmp ne ptr %{{.*}}, null
// UNSPECIFIED-NOT: {{^}}}
// UNSPECIFIED: @{{.*}}retainA{{.*}}(

// NULLABLE-LABEL: define {{.*}} @{{.*}}__synthesizedBaseGetterAccessor_nullable{{(Ev|@)}}
// NULLABLE-NOT: {{^}}}
// NULLABLE: icmp ne ptr %{{.*}}, null
// NULLABLE-NOT: {{^}}}
// NULLABLE: @{{.*}}retainA{{.*}}(

// NONNULL-LABEL: define {{.*}} @{{.*}}__synthesizedBaseGetterAccessor_nonnull{{(Ev|@)}}
// NONNULL-NOT: icmp
// NONNULL-NOT: {{^}}}
// NONNULL: @{{.*}}retainA{{.*}}(

// NULLABLE-RETAIN-LABEL: define {{.*}} @{{.*}}__synthesizedBaseGetterAccessor_nullableRetain{{(Ev|@)}}
// NULLABLE-RETAIN-NOT: icmp
// NULLABLE-RETAIN-NOT: {{^}}}
// NULLABLE-RETAIN: @{{.*}}retainB{{.*}}(

// NULLABLE-RETAIN-DEF-LABEL: define {{.*}} @{{.*}}__synthesizedBaseGetterAccessor_nullableRetainDefinition{{(Ev|@)}}
// NULLABLE-RETAIN-DEF-NOT: icmp
// NULLABLE-RETAIN-DEF-NOT: {{^}}}
// NULLABLE-RETAIN-DEF: @{{.*}}retainC{{.*}}(
