// REQUIRES: objc_interop
// REQUIRES: concurrency

// RUN: %empty-directory(%t)

// RUN: %sourcekitd-test -req=interface-gen %S/Inputs/gen_concurrency.swift -- %S/Inputs/gen_concurrency.swift -target %target-triple -I %t -Xfrontend -enable-experimental-concurrency | %FileCheck %s --check-prefix=SWIFT-GEN-INTERFACE

// Make sure we print @completionHandlerAsync when it was explicitly written by the user.
// SWIFT-GEN-INTERFACE-LABEL: class ClassWithAsyncAndHandler {
// SWIFT-GEN-INTERFACE:         @completionHandlerAsync("foo(_:)", completionHandlerIndex: 1)
// SWIFT-GEN-INTERFACE-NEXT:    internal func foo(_ operation: String, completionHandler handler: @escaping (Int) -> Void)
// SWIFT-GEN-INTERFACE:         internal func foo(_ operation: String) async -> Int

// RUN: %sourcekitd-test -req=interface-gen -using-swift-args -header %S/Inputs/header_concurrency.h -- %s -Xfrontend -enable-objc-interop -Xfrontend -enable-experimental-concurrency -import-objc-header %S/Inputs/header_concurrency.h -sdk %clang-importer-sdk | %FileCheck %s --check-prefix=OBJC-GEN-INTERFACE

// But don't print @completionHandlerAsync if it was implicitly added to an imported Clang decl (rdar://76685011).
// OBJC-GEN-INTERFACE-LABEL: class ClassWithHandlerMethod {
// OBJC-GEN-INTERFACE-NOT:     @completionHandlerAsync
// OBJC-GEN-INTERFACE:         func method(withHandler operation: String!, completionHandler handler: ((Int) -> Void)!)
// OBJC-GEN-INTERFACE:         func method(withHandler operation: String!) async -> Int
