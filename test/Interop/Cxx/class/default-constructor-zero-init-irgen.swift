// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -cxx-interoperability-mode=default -emit-ir -I %t%{fs-sep}Inputs %t%{fs-sep}test.swift -Xcc -fignore-exceptions | %FileCheck %s

// Swift value-initializes C++ types like C++ does: the backing memory is
// zeroed before calling the default constructor only if that constructor is
// not user-provided.

//--- Inputs/module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- Inputs/test.h
struct UserProvided {
  UserProvided();
  unsigned size;
  char storage[100];
};

struct UserProvidedOutOfLineDefault {
  UserProvidedOutOfLineDefault();
  unsigned size = 1;
  char storage[100];
};
inline UserProvidedOutOfLineDefault::UserProvidedOutOfLineDefault() = default;

struct DefaultedInClass {
  DefaultedInClass() = default;
  unsigned size = 1;
  char storage[100];
};

struct Implicit {
  unsigned size = 1;
  char storage[100];
};

struct ImplicitWithUserProvidedMember {
  UserProvided member;
  unsigned extra;
};

//--- test.swift
import Test

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4test16makeUserProvided{{.*}}"(
// CHECK-NOT: call void @llvm.memset
// CHECK: call {{.*}}UserProvided
// CHECK: ret void
public func makeUserProvided() -> UserProvided {
  return UserProvided()
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4test32makeUserProvidedOutOfLineDefault{{.*}}"(
// CHECK-NOT: call void @llvm.memset
// CHECK: call {{.*}}UserProvidedOutOfLineDefault
// CHECK: ret void
public func makeUserProvidedOutOfLineDefault() -> UserProvidedOutOfLineDefault {
  return UserProvidedOutOfLineDefault()
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4test20makeDefaultedInClass{{.*}}"(
// CHECK: call void @llvm.memset{{.*}}(ptr {{.*}}, i8 0, i64 104, i1 false)
// CHECK: call {{.*}}DefaultedInClass
// CHECK: ret void
public func makeDefaultedInClass() -> DefaultedInClass {
  return DefaultedInClass()
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4test12makeImplicit{{.*}}"(
// CHECK: call void @llvm.memset{{.*}}(ptr {{.*}}, i8 0, i64 104, i1 false)
// CHECK: call {{.*}}Implicit
// CHECK: ret void
public func makeImplicit() -> Implicit {
  return Implicit()
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4test34makeImplicitWithUserProvidedMember{{.*}}"(
// CHECK: call void @llvm.memset{{.*}}(ptr {{.*}}, i8 0, i64 108, i1 false)
// CHECK: call {{.*}}ImplicitWithUserProvidedMember
// CHECK: ret void
public func makeImplicitWithUserProvidedMember() -> ImplicitWithUserProvidedMember {
  return ImplicitWithUserProvidedMember()
}
