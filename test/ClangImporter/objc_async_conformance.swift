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

// try to conform to an objc protocol that has both a sync and async requirement
// that has the same name, and both requirements are optional.
class C2 : NSObject, OptionalObserver {}
extension C2 {
  func hello(_ session: NSObject) -> Bool { true }
}

// a version of C2 that requires both sync and async methods (differing only by
// completion handler) in ObjC.
class C3 : NSObject, RequiredObserver {}
extension C3 {
  func hello() -> Bool { true }
  func hello() async -> Bool { true }
}

// the only way to conform to 'RequiredObserver' in Swift is to not use 'async'
class C4 : NSObject, RequiredObserver {}
extension C4 {
  func hello() -> Bool { true }
  func hello(_ completion : @escaping (Bool) -> Void) -> Void { completion(true) }
}

protocol Club : ObjCClub {}

class ConformsToSync : NSObject, Club {
  func activate( completion: @escaping ( Error? ) -> Void ) { }
}

///////
// selector conflicts

// attempting to satisfy the ObjC async requirement in two ways simultaenously
// is problematic due to a clash in selector names on this ObjC-compatible type
class SelectorConflict : NSObject, RequiredObserverOnlyCompletion {
  func hello() async -> Bool { true } // expected-note {{method 'hello()' declared here}}

  // expected-error@+1 {{method 'hello' with Objective-C selector 'hello:' conflicts with method 'hello()' with the same Objective-C selector}}
  func hello(_ completion : @escaping (Bool) -> Void) -> Void { completion(true) }
}

// making either one of the two methods nonobjc fixes it:
class SelectorOK1 : NSObject, RequiredObserverOnlyCompletion {
  @nonobjc func hello() async -> Bool { true }
  func hello(_ completion : @escaping (Bool) -> Void) -> Void { completion(true) }
}

class SelectorOK2 : NSObject, RequiredObserverOnlyCompletion {
  func hello() async -> Bool { true }
  @nonobjc func hello(_ completion : @escaping (Bool) -> Void) -> Void { completion(true) }
}

// additional coverage for situation like C4, where the method names don't
// clash on the ObjC side, but they do on Swift side, BUT their ObjC selectors
// differ, so it's OK.
class Rock : NSObject, Rollable {
  func roll(completionHandler: @escaping () -> Void) { completionHandler() }
  func roll() { roll(completionHandler: {}) }
}

// additional coverage for a situation where only an argument label differs, excluding the completion handler.
final class Moon : LabellyProtocol {
  func myMethod(_ value: Int, foo: Int) {}
  func myMethod(_ value: Int, newFoo foo: Int, completion: @escaping (Error?) -> Void) {}
}

// Crash involving actor isolation checking.
class C5 {
  @MainActor @objc var allOperations: [String] = []
}

class C6: C5, ServiceProvider {
  @MainActor func allOperations() async -> [String] { [] }
}
