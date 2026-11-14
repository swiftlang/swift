// REQUIRES: swift_swift_parser

// RUN: %target-typecheck-verify-swift -swift-version 6 -disable-availability-checking -parse-as-library -plugin-path %swift-plugin-dir

// REQUIRES: observation
// REQUIRES: concurrency
// REQUIRES: objc_interop
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Foundation
import Observation

// 'nonisolated(unsafe)' on an observed property is copied to its storage,
// where it takes effect, so it doesn't warn that it has no effect.
@MainActor @Observable
final class IsolatedObservable {
  nonisolated(unsafe) var unsafeValue = false
  nonisolated(unsafe) var unsafeWithDidSet = false {
    didSet { didSetCount += 1 }
  }
  nonisolated(unsafe) var didSetCount = 0
  var isolatedValue = 0
}

@MainActor @Observable
final class IsolatedObjCObservable: NSObject, Sendable {
  @objc nonisolated(unsafe) public var isOnboarding: Bool = false {
    didSet { _ = isOnboarding }
  }
}

nonisolated func readAndWrite(_ object: IsolatedObservable) {
  object.unsafeValue.toggle()
  object.unsafeWithDidSet = object.unsafeValue
}

// 'nonisolated' on mutable storage of a 'Sendable' type is diagnosed on the
// observed property, not on its storage.
@MainActor @Observable
final class IsolatedObservableWithNonisolated {
  nonisolated var value = 0 // expected-error {{'nonisolated' cannot be applied to mutable stored properties}} {{14-14=(unsafe)}}
  // expected-note@-1 {{convert 'value' to a 'let' constant or consider declaring it 'nonisolated(unsafe)' if manually managing concurrency safety}}
}

// A non-'Sendable' type can have 'nonisolated' mutable storage.
@Observable
final class NonSendableObservable {
  nonisolated var value = 0
}

// A computed property that wasn't rewritten by a macro still warns.
@MainActor
final class HandWrittenInitAccessor {
  private nonisolated(unsafe) var _value: Int
  nonisolated(unsafe) var value: Int { // expected-warning {{'nonisolated(unsafe)' has no effect on property 'value', consider using 'nonisolated'}}
    @storageRestrictions(initializes: _value)
    init { _value = newValue }
    get { _value }
    set { _value = newValue }
  }

  init() { value = 0 }
}
