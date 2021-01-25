// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules -enable-experimental-concurrency %s -verify

// REQUIRES: objc_interop
// REQUIRES: concurrency
import Foundation
import ObjCConcurrency

// Conform via async method
class C1: ConcurrentProtocol {
  func askUser(toSolvePuzzle puzzle: String) async throws -> String { "" }

  func askUser(toJumpThroughHoop hoop: String) async -> String { "hello" }
}
