// RUN: %target-swift-frontend -emit-ir -parse-as-library -target %target-cpu-apple-macosx13.0 %s | %FileCheck %s --check-prefix=OLD
// RUN: %target-swift-frontend -emit-ir -parse-as-library -target %target-cpu-apple-macosx15.0 %s | %FileCheck %s --check-prefix=NEW

// REQUIRES: OS=macosx
// REQUIRES: CPU=arm64 || CPU=x86_64

// A generic back-deployed type with inverse requirements, like UniqueBox, is
// only demangleable by a Swift 6.0 or newer runtime, and the runtimes that the
// type is back-deployed to don't know about the client's copy of it anyway.
// When the deployment target's runtime is older than 6.0, the metadata has to
// come from the type's accessor, not from a mangled name.

@inline(never)
public func g<T: ~Copyable>(_: T.Type) -> Int { MemoryLayout<T>.size }

// OLD-LABEL: define {{.*}} @"$s{{.*}}3boxSiyF"
// OLD-NOT: __swift_instantiateConcreteTypeFromMangledName
// OLD: call {{.*}} @"$ss9UniqueBoxVySiGMaTwa"

// NEW-LABEL: define {{.*}} @"$s{{.*}}3boxSiyF"
// NEW: call {{.*}} @__swift_instantiateConcreteTypeFromMangledName
@inline(never)
public func box() -> Int { g(UniqueBox<Int>.self) }
