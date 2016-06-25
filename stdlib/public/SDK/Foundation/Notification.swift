//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module


@_silgen_name("__NSNotificationCreate")
internal func __NSNotificationCreate(_ name: NSString, _ object: AnyObject?, _ userInfo: AnyObject?) -> NSNotification

@_silgen_name("__NSNotificationUserInfo")
internal func __NSNotificationUserInfo(_ notif: NSNotification) -> AnyObject?

/**
 `Notification` encapsulates information broadcast to observers via a `NotificationCenter`.
*/
public struct Notification : ReferenceConvertible, Equatable, Hashable {
    public typealias ReferenceType = NSNotification
    
    /// A tag identifying the notification.
    public var name: Name
    
    /// An object that the poster wishes to send to observers.
    ///
    /// Typically this is the object that posted the notification.
    public var object: AnyObject?
    
    /// Storage for values or objects related to this notification.
    public var userInfo: [String : Any]?
    
    /// Initialize a new `Notification`.
    ///
    /// The default value for `userInfo` is nil.
    public init(name: Name, object: AnyObject? = nil, userInfo: [String : Any]? = nil) {
        self.name = name
        self.object = object
        self.userInfo = userInfo
    }

    public var hashValue: Int {
        return name.rawValue.hash
    }
    
    public var description: String {
        return "name = \(name.rawValue), object = \(object), userInfo = \(userInfo)"
    }
    
    public var debugDescription: String {
        return description
    }

    // FIXME: Handle directly via API Notes
    public typealias Name = NSNotification.Name
}

/// Compare two notifications for equality.
///
/// - note: Notifications that contain non NSObject values in userInfo will never compare as equal. This is because the type information is not preserved in the `userInfo` dictionary.
public func ==(lhs: Notification, rhs: Notification) -> Bool {
    if lhs.name.rawValue != rhs.name.rawValue {
        return false
    }
    if let lhsObj = lhs.object {
        if let rhsObj = rhs.object {
            if lhsObj !== rhsObj {
                return false
            }
        } else {
            return false
        }
    } else if rhs.object != nil {
        return false
    }
    if let lhsUserInfo = lhs.userInfo {
        if let rhsUserInfo = rhs.userInfo {
            if lhsUserInfo.count != rhsUserInfo.count {
                return false
            }
            return _NSUserInfoDictionary.compare(lhsUserInfo, rhsUserInfo)
        } else {
            return false
        }
    } else if rhs.userInfo != nil {
        return false
    }
    return true
}

extension Notification : _ObjectiveCBridgeable {
    public static func _isBridgedToObjectiveC() -> Bool {
        return true
    }
    
    public static func _getObjectiveCType() -> Any.Type {
        return NSNotification.self
    }
    
    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSNotification {
        if let info = userInfo {
            return __NSNotificationCreate(name.rawValue as NSString, object, _NSUserInfoDictionary.bridgeValue(from: info))
        }

        return NSNotification(name: name, object: object, userInfo: nil)    
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSNotification, result: inout Notification?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge type")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ x: NSNotification, result: inout Notification?) -> Bool {
        if let userInfo = __NSNotificationUserInfo(x) {
            if let info : [String : Any]? = _NSUserInfoDictionary.bridgeReference(from: userInfo) {
                result = Notification(name: x.name, object: x.object, userInfo: info)
                return true
            } else {
                result = nil
                return false // something terrible went wrong...
            }
        } else {
            result = Notification(name: x.name, object: x.object, userInfo: nil)
        }
        
        return true
    }

    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSNotification?) -> Notification {
        var result: Notification? = nil
        _forceBridgeFromObjectiveC(source!, result: &result)
        return result!
    }
}

extension NotificationCenter {
  // Note: rdar://problem/26177286
  // Note: should be removed with Foundation epoch 8 along with the signature
  // below.
  @nonobjc public final func addObserver(_ observer: AnyObject, selector aSelector: Selector, name aName: Notification.Name, object anObject: AnyObject?) {
    self.addObserver(observer, selector: aSelector, name: aName.rawValue, object: anObject)
  }
}
