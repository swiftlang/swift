// RUN: %target-swift-frontend -enable-experimental-feature IsolatedDeinit -target %target-swift-5.5-abi-triple -emit-silgen -verify %s
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) -DEMIT_IFACE %s -enable-experimental-feature IsolatedDeinit -target %target-swift-5.5-abi-triple -module-name IsolatedDeinitCompatibility
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -enable-experimental-feature IsolatedDeinit -target %target-swift-5.5-abi-triple -module-name IsolatedDeinitCompatibility
// RUN: %FileCheck %s < %t.swiftinterface

// REQUIRES: swift_feature_IsolatedDeinit

// MARK: Sync deinit in class

// CHECK-NOT: #
// CHECK: open class SyncClassDefaultOpen {
// CHECK-NOT: #
// CHECK: {{(@objc )?}}deinit
// CHECK-NOT: #
// CHECK: }
open class SyncClassDefaultOpen {
    deinit {}
}

// CHECK-NOT: #
// CHECK: public class SyncClassDefaultPublic {
// CHECK-NOT: #
// CHECK: {{(@objc )?}}deinit
// CHECK-NOT: #
// CHECK: }
public class SyncClassDefaultPublic {
    deinit {}
}

// CHECK: #if {{.*}}$IsolatedDeinit
// CHECK-NOT: #
// CHECK: open class SyncClassGlobalActorOpen {
// CHECK-NOT: #
// CHECK: {{(@objc )?}}@_Concurrency.MainActor deinit
// CHECK-NOT: #
// CHECK: }
// CHECK: #endif
open class SyncClassGlobalActorOpen {
    @MainActor deinit {}
}

// CHECK-NOT: #
// CHECK: public class SyncClassGlobalActorPublic {
// CHECK-NOT: #
// CHECK: #if {{.*}}$IsolatedDeinit
// CHECK-NOT: #
// CHECK: {{(@objc )?}}@_Concurrency.MainActor deinit
// CHECK: }
public class SyncClassGlobalActorPublic {
    @MainActor deinit {}
}

// CHECK: #if {{.*}}$IsolatedDeinit
// CHECK-NOT: #
// CHECK: @_Concurrency.MainActor open class SyncClassIsolatedOpen {
// CHECK-NOT: #
// CHECK: {{(@objc )?}}isolated deinit
// CHECK-NOT: #
// CHECK: }
// CHECK: #endif
@MainActor
open class SyncClassIsolatedOpen {
    isolated deinit {}
}

// CHECK-NOT: #
// CHECK: @_Concurrency.MainActor public class SyncClassIsolatedPublic {
// CHECK-NOT: #
// CHECK: #if {{.*}}$IsolatedDeinit
// CHECK-NOT: #
// CHECK: {{(@objc )?}}isolated deinit
// CHECK: }
@MainActor
public class SyncClassIsolatedPublic {
    isolated deinit {}
}

// CHECK: #if {{.*}}$IsolatedDeinit
// CHECK-NOT: #
// CHECK: @_Concurrency.MainActor open class SyncClassNonisolatedOpen {
// CHECK-NOT: #
// CHECK: {{(@objc )?}}nonisolated deinit
// CHECK-NOT: #
// CHECK: }
// CHECK: #endif
@MainActor
open class SyncClassNonisolatedOpen {
    nonisolated deinit {}
}

// CHECK-NOT: #
// CHECK: @_Concurrency.MainActor public class SyncClassNonisolatedPublic {
// CHECK-NOT: #
// CHECK: #if {{.*}}$IsolatedDeinit
// CHECK-NOT: #
// CHECK: {{(@objc )?}}nonisolated deinit
// CHECK: }
@MainActor
public class SyncClassNonisolatedPublic {
    nonisolated deinit {}
}

// MARK: Sync deinit in actor

// CHECK-NOT: #
// CHECK: public actor SyncActorDefaultPublic {
// CHECK-NOT: #
// CHECK: {{(@objc )?}}deinit
// CHECK-NOT: #
// CHECK: }
public actor SyncActorDefaultPublic {
    deinit {}
}

// CHECK-NOT: #
// CHECK: public actor SyncActorGlobalActorPublic {
// CHECK-NOT: #
// CHECK: #if {{.*}}$IsolatedDeinit
// CHECK-NOT: #
// CHECK: {{(@objc )?}}@_Concurrency.MainActor deinit
// CHECK: }
public actor SyncActorGlobalActorPublic {
    @MainActor deinit {}
}

// CHECK-NOT: #
// CHECK: public actor SyncActorIsolatedPublic {
// CHECK-NOT: #
// CHECK: #if {{.*}}$IsolatedDeinit
// CHECK-NOT: #
// CHECK: {{(@objc )?}}isolated deinit
// CHECK: }
public actor SyncActorIsolatedPublic {
    isolated deinit {}
}

// CHECK-NOT: #
// CHECK: public actor SyncActorNonisolatedPublic {
// CHECK-NOT: #
// CHECK: #if {{.*}}$IsolatedDeinit
// CHECK-NOT: #
// CHECK: {{(@objc )?}}nonisolated deinit
// CHECK: }
public actor SyncActorNonisolatedPublic {
    nonisolated deinit {}
}

// MARK: - Open actor

// Check that open actors are not allowed
// If they become allowed in the future, extra test cases need to be added to this test
#if !EMIT_IFACE
open actor OpenActor {} // expected-error {{only classes and overridable class members can be declared 'open'; use 'public'}}
#endif
