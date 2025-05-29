// REQUIRES: objc_interop
// REQUIRES: concurrency
// REQUIRES: swift_feature_SendableCompletionHandlers

// RUN: %empty-directory(%t)

// RUN: %sourcekitd-test -req=interface-gen %S/../Inputs/concurrency/gen_concurrency.swift -- %S/../Inputs/concurrency/gen_concurrency.swift -target %target-triple -I %t -enable-experimental-feature SendableCompletionHandlers | %FileCheck %s --check-prefix=SWIFT-GEN-INTERFACE

// Make sure we print @available when it was explicitly written by the user.
// SWIFT-GEN-INTERFACE-LABEL: class ClassWithAsyncAndHandler {
// SWIFT-GEN-INTERFACE:         @available(*, renamed: "foo(_:)")
// SWIFT-GEN-INTERFACE-NEXT:    internal func foo(_ operation: String, completionHandler handler: @escaping (Int) -> Void)
// SWIFT-GEN-INTERFACE:         internal func foo(_ operation: String) async -> Int

// SWIFT-GEN-INTERFACE:         @MainActor internal func mainActorMethod()


// RUN: %sourcekitd-test -req=interface-gen -using-swift-args -header %S/../Inputs/concurrency/header_concurrency.h -- %s -Xfrontend -enable-objc-interop -import-objc-header %S/../Inputs/concurrency/header_concurrency.h -sdk %clang-importer-sdk -enable-experimental-feature SendableCompletionHandlers | %FileCheck %s --check-prefix=OBJC-GEN-INTERFACE

// But don't print @available if it was implicitly added to an imported Clang decl (rdar://76685011).
// OBJC-GEN-INTERFACE-LABEL: class ClassWithHandlerMethod {
// OBJC-GEN-INTERFACE-NOT:     @available
// OBJC-GEN-INTERFACE:         func method(withHandler operation: String!, completionHandler handler: (@Sendable (Int) -> Void)!)
// OBJC-GEN-INTERFACE:         func method(withHandler operation: String!) async -> Int

// OBJC-GEN-INTERFACE:         @MainActor open func mainActorMethod()
