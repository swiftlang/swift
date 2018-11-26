//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Intents
import Foundation

#if os(iOS) || os(watchOS)

@available(iOS 12.0, watchOS 5.0, *)
public enum INShortcut : ReferenceConvertible {
  public typealias ReferenceType = INShortcutReference

  case intent(INIntent)
  case userActivity(NSUserActivity)

  init(from objcShortcut: INShortcutReference) {
    if let intent = objcShortcut.intent {
      self = .intent(intent)
    } else if let userActivity = objcShortcut.userActivity {
      self = .userActivity(userActivity)
    } else {
      fatalError("INShortcutReference object must have either intent or userActivity")
    }
  }
}

// Convenience initializers, to mimic the ObjC initializer API
@available(iOS 12.0, watchOS 5.0, *)
public extension INShortcut {
  public init?(intent: INIntent) {
    // use the ObjC initializer, to re-use its validation of the intent
    guard let ref = INShortcutReference(intent: intent) else { return nil }
    self.init(from: ref)
  }
  public init(userActivity: NSUserActivity) {
    self = .userActivity(userActivity)
  }
}

// Convenience properties, to mimic the ObjC API
@available(iOS 12.0, watchOS 5.0, *)
public extension INShortcut {
  public var intent: INIntent? {
    guard case let .intent(intent) = self else { return nil }
    return intent
  }
  public var userActivity: NSUserActivity? {
    guard case let .userActivity(userActivity) = self else { return nil }
    return userActivity
  }
}

@available(iOS 12.0, watchOS 5.0, *)
extension INShortcut : CustomStringConvertible {
  public var description: String {
    return reference.description
  }
}

@available(iOS 12.0, watchOS 5.0, *)
extension INShortcut : CustomDebugStringConvertible {
  public var debugDescription: String {
    return reference.debugDescription
  }
}

@available(iOS 12.0, watchOS 5.0, *)
extension INShortcut : Hashable {
  public func hash(into hasher: inout Hasher) {
    reference.hash(into: &hasher)
  }
}

@available(iOS 12.0, watchOS 5.0, *)
extension INShortcut : Equatable {}

@available(iOS 12.0, watchOS 5.0, *)
private extension INShortcut {
  fileprivate var reference: INShortcutReference {
    switch self {
    case .intent(let intent):
      return INShortcutReference(intent: intent)!
    case .userActivity(let userActivity):
      return INShortcutReference(userActivity: userActivity)
    }
  }
}

@available(iOS 12.0, watchOS 5.0, *)
extension INShortcut : _ObjectiveCBridgeable {
  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> INShortcutReference {
    return self.reference
  }

  public static func _forceBridgeFromObjectiveC(_ source: INShortcutReference, result: inout INShortcut?) {
    if !_conditionallyBridgeFromObjectiveC(source, result: &result) {
      fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
    }
  }

  public static func _conditionallyBridgeFromObjectiveC(_ source: INShortcutReference, result: inout INShortcut?) -> Bool {
    result = INShortcut(from: source)
    return true
  }

  public static func _unconditionallyBridgeFromObjectiveC(_ source: INShortcutReference?) -> INShortcut {
    guard let src = source else { fatalError("Missing source") }
    return INShortcut(from: src)
  }
}

#endif
