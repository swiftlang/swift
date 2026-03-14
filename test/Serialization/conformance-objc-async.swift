// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-emit-module-interface(%t/Conformance.swiftinterface) -target %target-swift-5.5-abi-triple -module-name Conformance -I %t %t/Conformance.swift
// RUN: %target-swift-emit-module-interface(%t/Conformance.swiftinterface) -target %target-swift-5.5-abi-triple -module-name Conformance -experimental-lazy-typecheck -I %t %t/Conformance.swift
// RUN: %target-swift-frontend -compile-module-from-interface %t/Conformance.swiftinterface -module-name Conformance -o /dev/null -I %t
// REQUIRES: objc_interop
// REQUIRES: concurrency

//--- module.modulemap
module ObjCProto {
  header "objc_proto.h"
  export *
}

//--- objc_proto.h
@protocol Doable
- (void)doItWithCompletion:(void (^)())completion;
@end

//--- Conformance.swift
import ObjCProto

public final class DoableWithCompletionHandler: Doable {
  // Matches synchronous requirement
  public func doIt(completion: @escaping () -> Void) {}
}

public final class DoableWithAsync: Doable {
  // Matches async requirement
  public func doIt() async {}
}

public final class DoableWithCompletionHandlerAndAsyncCandidate: Doable {
  // Matches synchronous requirement
  public func doIt(completion: @escaping () -> Void) {}

  // Near-miss for async requirement
  public func doIt() async -> Int {}
}

public final class DoableWithCompletionHandlerAndAsyncCandidate2: Doable {
  // Near-miss for async requirement
  public func doIt() async -> Int {}

  // Matches synchronous requirement
  public func doIt(completion: @escaping () -> Void) {}
}

public final class DoableWithCompletionHandlerAndMultipleAsyncCandidates: Doable {
  // Matches synchronous requirement
  public func doIt(completion: @escaping () -> Void) {}

  // Multiple near-misses for async requirement
  public func doIt() async -> Int {}
  public func doIt() async -> Double {}
}

public final class DoableWithCompletionHandlerAndMultipleAsyncCandidates2: Doable {
  // Multiple near-misses for async requirement
  public func doIt() async -> Int {}
  public func doIt() async -> Double {}

  // Matches synchronous requirement
  public func doIt(completion: @escaping () -> Void) {}
}

public final class DoableWithAsyncAndCompletionHandlerCandidate: Doable {
  // Matches async requirement
  public func doIt() async {}

  // Near-miss for synchronous requirement
  public func doIt(reply: @escaping () -> Void) {}
}

public final class DoableWithAsyncAndCompletionHandlerCandidate2: Doable {
  // Near-miss for synchronous requirement
  public func doIt(reply: @escaping () -> Void) {}

  // Matches async requirement
  public func doIt() async {}
}

public final class DoableWithAsyncAndMultipleCompletionHandlerCandidates: Doable {
  // Matches async requirement
  public func doIt() async {}

  // Multiple near-misses for synchronous requirement
  public func doIt(reply: @escaping () -> Void) {}
  public func doIt(otherReply: @escaping () -> Void) {}
}

public final class DoableWithAsyncAndMultipleCompletionHandlerCandidates2: Doable {
  // Multiple near-misses for synchronous requirement
  public func doIt(otherReply: @escaping () -> Void) {}
  public func doIt(reply: @escaping () -> Void) {}

  // Matches async requirement
  public func doIt() async {}
}
