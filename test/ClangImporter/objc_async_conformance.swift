// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules -disable-availability-checking  %s -verify

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

// attempting to satisfy the ObjC async requirement in two ways simultaneously
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

// can declare an @objc protocol with both selectors...
@objc protocol SelectorBothAsyncProto {
  @objc(helloWithCompletion:)
  func hello() async -> Bool

  @available(*, renamed: "hello()")
  @objc(helloWithCompletion:)
  func hello(completion: @escaping (Bool) -> Void)
}

// and conform by implementing either one...
class SelectorBothAsync1: NSObject, SelectorBothAsyncProto {
  func hello() async -> Bool { true }
}
class SelectorBothAsync2: NSObject, SelectorBothAsyncProto {
  func hello(completion: @escaping (Bool) -> Void) { completion(true) }
}

// but not without declaring the async alternative.
@objc protocol BadSelectorBothAsyncProto {
  @objc(helloWithCompletion:)
  func hello() async -> Bool // expected-note {{method 'hello()' declared here}}

  @objc(helloWithCompletion:)
  func hello(completion: @escaping (Bool) -> Void) // expected-warning {{method 'hello(completion:)' with Objective-C selector 'helloWithCompletion:' conflicts with method 'hello()' with the same Objective-C selector; this is an error in the Swift 6 language mode}}
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

extension ImplementsLoadable: @retroactive Loadable {
  public func loadStuff(withOtherIdentifier otherIdentifier: Int, reply: @escaping () -> Void) {}
}

class ImplementsDictionaryLoader1: DictionaryLoader {
  func loadDictionary(completionHandler: @escaping ([String: NSNumber]?) -> Void) {}
}

// expected-error@+2 {{type 'ImplementsDictionaryLoader2' does not conform to protocol 'DictionaryLoader'}}
// expected-note@+1 {{add stubs for conformance}}
class ImplementsDictionaryLoader2: DictionaryLoader {
  func loadDictionary(completionHandler: @escaping ([String: Any]?) -> Void) {} // expected-note {{candidate has non-matching type '(@escaping ([String : Any]?) -> Void) -> ()'}}
}

// expected-error@+2 {{type 'ImplementsDictionaryLoader3' does not conform to protocol 'DictionaryLoader'}}
// expected-note@+1 {{add stubs for conformance}}
class ImplementsDictionaryLoader3: DictionaryLoader {
  func loadDictionary(completionHandler: @escaping ([String: NSNumber?]?) -> Void) {} // expected-note {{candidate has non-matching type '(@escaping ([String : NSNumber?]?) -> Void) -> ()'}}
}

// expected-error@+2 {{type 'ImplementsDictionaryLoader4' does not conform to protocol 'DictionaryLoader'}}
// expected-note@+1 {{add stubs for conformance}}
class ImplementsDictionaryLoader4: DictionaryLoader {
  func loadDictionary(completionHandler: @escaping ([String: Int]?) -> Void) {} // expected-note {{candidate has non-matching type '(@escaping ([String : Int]?) -> Void) -> ()'}}
}

class ImplementsFloatLoader: FloatLoader {
  public func loadFloat(completionHandler: @escaping (Float) -> Void) {}
}

class ImplementsFloatLoader2: FloatLoader {
  public func loadFloat(withCompletionHandler completionHandler: @escaping (Float) -> Void) {}
  // expected-warning@-1 {{instance method 'loadFloat(withCompletionHandler:)' nearly matches optional requirement 'loadFloat(completionHandler:)' of protocol 'FloatLoader'}}
  // expected-note@-2 {{rename to 'loadFloat(completionHandler:)' to satisfy this requirement}}
  // expected-note@-3 {{move 'loadFloat(withCompletionHandler:)' to an extension to silence this warning}}
}
