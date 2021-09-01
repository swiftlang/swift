// REQUIRES: no_asan
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch

// REQUIRES: CPU=x86_64
// REQUIRES: OS=macosx

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// RUN: %empty-directory(%t)

// RUN: %target-build-swift -Xfrontend -enable-anonymous-context-mangled-names %S/Inputs/ConcurrencyTypes.swift -parse-as-library -emit-module -emit-library -module-name TypesToReflect -o %t/%target-library-name(TypesToReflect) -target x86_64-apple-macosx12.0
// RUN: %target-build-swift -Xfrontend -enable-anonymous-context-mangled-names %S/Inputs/ConcurrencyTypes.swift %S/Inputs/main.swift -emit-module -emit-executable -module-name TypesToReflect -o %t/TypesToReflect -target x86_64-apple-macosx12.0

// For macOS versions before 12.0, the mangling for concurrency-related
// types cannot be used to create type metadata.

// RUN: %target-swift-reflection-dump -binary-filename %t/%target-library-name(TypesToReflect) | %FileCheck %s
// RUN: %target-swift-reflection-dump -binary-filename %t/TypesToReflect | %FileCheck %s

// CHECK: FIELDS:
// CHECK: =======
// CHECK: TypesToReflect.UsesConcurrency
// CHECK: ------------------
// CHECK: mainActorFunction: @Swift.MainActor () -> ()
// CHECK: (function
// CHECK:   (global-actor
// CHECK:     (class Swift.MainActor))

// CHECK: actorIsolatedFunction: (isolated TypesToReflect.SomeActor) -> ()
// CHECK: (function
// CHECK:   (parameters
// CHECK:     isolated
// CHECK:     (class TypesToReflect.SomeActor))
