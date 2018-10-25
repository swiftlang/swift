//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2017 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module
import ObjectiveC

// This exists to allow for dynamic dispatch on KVO methods added to NSObject.
// Extending NSObject with these methods would disallow overrides.
public protocol _KeyValueCodingAndObserving {}
extension NSObject : _KeyValueCodingAndObserving {}

public struct NSKeyValueObservedChange<Value> {
    public typealias Kind = NSKeyValueChange
    public let kind: Kind
    ///newValue and oldValue will only be non-nil if .new/.old is passed to `observe()`. In general, get the most up to date value by accessing it directly on the observed object instead.
    public let newValue: Value?
    public let oldValue: Value?
    ///indexes will be nil unless the observed KeyPath refers to an ordered to-many property
    public let indexes: IndexSet?
    ///'isPrior' will be true if this change observation is being sent before the change happens, due to .prior being passed to `observe()`
    public let isPrior:Bool
}

///Conforming to NSKeyValueObservingCustomization is not required to use Key-Value Observing. Provide an implementation of these functions if you need to disable auto-notifying for a key, or add dependent keys
public protocol NSKeyValueObservingCustomization : NSObjectProtocol {
    static func keyPathsAffectingValue(for key: AnyKeyPath) -> Set<AnyKeyPath>
    static func automaticallyNotifiesObservers(for key: AnyKeyPath) -> Bool
}

fileprivate extension NSObject {
    
    @objc class func __old_unswizzled_automaticallyNotifiesObservers(forKey key: String?) -> Bool {
        fatalError("Should never be reached")
    }
    
    @objc class func __old_unswizzled_keyPathsForValuesAffectingValue(forKey key: String?) -> Set<String> {
        fatalError("Should never be reached")
    }

}

// NOTE: older overlays called this _KVOKeyPathBridgeMachinery. The two
// must coexist, so it was renamed. The old name must not be used in the
// new runtime.
@objc private class __KVOKeyPathBridgeMachinery : NSObject {
    @nonobjc static var keyPathTable: [String : AnyKeyPath] = {
        /*
         Move all our methods into place. We want the following:
         __KVOKeyPathBridgeMachinery's automaticallyNotifiesObserversForKey:, and keyPathsForValuesAffectingValueForKey: methods replaces NSObject's versions of them
         NSObject's automaticallyNotifiesObserversForKey:, and keyPathsForValuesAffectingValueForKey: methods replace NSObject's __old_unswizzled_* methods
         NSObject's _old_unswizzled_* methods replace __KVOKeyPathBridgeMachinery's methods, and are never invoked
         */
        threeWaySwizzle(#selector(NSObject.keyPathsForValuesAffectingValue(forKey:)), with: #selector(NSObject.__old_unswizzled_keyPathsForValuesAffectingValue(forKey:)))
        threeWaySwizzle(#selector(NSObject.automaticallyNotifiesObservers(forKey:)), with: #selector(NSObject.__old_unswizzled_automaticallyNotifiesObservers(forKey:)))
        
        return [:]
    }()
    
    /// Performs a 3-way swizzle between `NSObject` and `__KVOKeyPathBridgeMachinery`.
    ///
    /// The end result of this swizzle is the following:
    /// * `NSObject.selector` contains the IMP from `__KVOKeyPathBridgeMachinery.selector`
    /// * `NSObject.unswizzledSelector` contains the IMP from the original `NSObject.selector`.
    /// * __KVOKeyPathBridgeMachinery.selector` contains the (useless) IMP from `NSObject.unswizzledSelector`.
    ///
    /// This swizzle is done in a manner that modifies `NSObject.selector` last, in order to ensure thread safety
    /// (by the time `NSObject.selector` is swizzled, `NSObject.unswizzledSelector` will contain the original IMP)
    private static func threeWaySwizzle(_ selector: Selector, with unswizzledSelector: Selector) {
        let rootClass: AnyClass = NSObject.self
        let bridgeClass: AnyClass = __KVOKeyPathBridgeMachinery.self
        
        // Swap bridge.selector <-> NSObject.unswizzledSelector
        let unswizzledMethod = class_getClassMethod(rootClass, unswizzledSelector)!
        let bridgeMethod = class_getClassMethod(bridgeClass, selector)!
        method_exchangeImplementations(unswizzledMethod, bridgeMethod)
        
        // Swap NSObject.selector <-> NSObject.unswizzledSelector
        // NSObject.unswizzledSelector at this point contains the bridge IMP
        let rootMethod = class_getClassMethod(rootClass, selector)!
        method_exchangeImplementations(rootMethod, unswizzledMethod)
    }
    
    @nonobjc static var keyPathTableLock = NSLock()
    
    @nonobjc fileprivate static func _bridgeKeyPath(_ keyPath: __owned AnyKeyPath) -> String {
        guard let keyPathString = keyPath._kvcKeyPathString else { fatalError("Could not extract a String from KeyPath \(keyPath)") }
        __KVOKeyPathBridgeMachinery.keyPathTableLock.lock()
        defer { __KVOKeyPathBridgeMachinery.keyPathTableLock.unlock() }
        __KVOKeyPathBridgeMachinery.keyPathTable[keyPathString] = keyPath
        return keyPathString
    }
    
    @nonobjc fileprivate static func _bridgeKeyPath(_ keyPath:String?) -> AnyKeyPath? {
        guard let keyPath = keyPath else { return nil }
        __KVOKeyPathBridgeMachinery.keyPathTableLock.lock()
        defer { __KVOKeyPathBridgeMachinery.keyPathTableLock.unlock() }
        let path = __KVOKeyPathBridgeMachinery.keyPathTable[keyPath]
        return path
    }
    
    @objc override class func automaticallyNotifiesObservers(forKey key: String) -> Bool {
        //This is swizzled so that it's -[NSObject automaticallyNotifiesObserversForKey:]
        if let customizingSelf = self as? NSKeyValueObservingCustomization.Type, let path = __KVOKeyPathBridgeMachinery._bridgeKeyPath(key) {
            return customizingSelf.automaticallyNotifiesObservers(for: path)
        } else {
            return self.__old_unswizzled_automaticallyNotifiesObservers(forKey: key) //swizzled to be NSObject's original implementation
        }
    }
    
    @objc override class func keyPathsForValuesAffectingValue(forKey key: String?) -> Set<String> {
        //This is swizzled so that it's -[NSObject keyPathsForValuesAffectingValueForKey:]
        if let customizingSelf = self as? NSKeyValueObservingCustomization.Type, let path = __KVOKeyPathBridgeMachinery._bridgeKeyPath(key!) {
            let resultSet = customizingSelf.keyPathsAffectingValue(for: path)
            return Set(resultSet.lazy.map {
                guard let str = $0._kvcKeyPathString else { fatalError("Could not extract a String from KeyPath \($0)") }
                return str
            })
        } else {
            return self.__old_unswizzled_keyPathsForValuesAffectingValue(forKey: key) //swizzled to be NSObject's original implementation
        }
    }
}

func _bridgeKeyPathToString(_ keyPath:AnyKeyPath) -> String {
    return __KVOKeyPathBridgeMachinery._bridgeKeyPath(keyPath)
}

func _bridgeStringToKeyPath(_ keyPath:String) -> AnyKeyPath? {
    return __KVOKeyPathBridgeMachinery._bridgeKeyPath(keyPath)
}

// NOTE: older overlays called this NSKeyValueObservation. We now use
// that name in the source code, but add an underscore to the runtime
// name to avoid conflicts when both are loaded into the same process.
@objc(_NSKeyValueObservation)
public class NSKeyValueObservation : NSObject {
    
    @nonobjc weak var object : NSObject?
    @nonobjc let callback : (NSObject, NSKeyValueObservedChange<Any>) -> Void
    @nonobjc let path : String
    
    //workaround for <rdar://problem/31640524> Erroneous (?) error when using bridging in the Foundation overlay
    @nonobjc static var swizzler : NSKeyValueObservation? = {
        let bridgeClass: AnyClass = NSKeyValueObservation.self
        let observeSel = #selector(NSObject.observeValue(forKeyPath:of:change:context:))
        let swapSel = #selector(NSKeyValueObservation._swizzle_me_observeValue(forKeyPath:of:change:context:))
        let rootObserveImpl = class_getInstanceMethod(bridgeClass, observeSel)!
        let swapObserveImpl = class_getInstanceMethod(bridgeClass, swapSel)!
        method_exchangeImplementations(rootObserveImpl, swapObserveImpl)
        return nil
    }()
    
    fileprivate init(object: NSObject, keyPath: AnyKeyPath, callback: @escaping (NSObject, NSKeyValueObservedChange<Any>) -> Void) {
        path = _bridgeKeyPathToString(keyPath)
        let _ = NSKeyValueObservation.swizzler
        self.object = object
        self.callback = callback
    }
    
    fileprivate func start(_ options: NSKeyValueObservingOptions) {
        object?.addObserver(self, forKeyPath: path, options: options, context: nil)
    }
    
    ///invalidate() will be called automatically when an NSKeyValueObservation is deinited
    @objc public func invalidate() {
        object?.removeObserver(self, forKeyPath: path, context: nil)
        object = nil
    }
    
    @objc func _swizzle_me_observeValue(forKeyPath keyPath: String?, of object: Any?, change: [NSString : Any]?, context: UnsafeMutableRawPointer?) {
        guard let ourObject = self.object, object as? NSObject == ourObject, let change = change else { return }
        let rawKind:UInt = change[NSKeyValueChangeKey.kindKey.rawValue as NSString] as! UInt
        let kind = NSKeyValueChange(rawValue: rawKind)!
        let notification = NSKeyValueObservedChange(kind: kind,
                                                    newValue: change[NSKeyValueChangeKey.newKey.rawValue as NSString],
                                                    oldValue: change[NSKeyValueChangeKey.oldKey.rawValue as NSString],
                                                    indexes: change[NSKeyValueChangeKey.indexesKey.rawValue as NSString] as! IndexSet?,
                                                    isPrior: change[NSKeyValueChangeKey.notificationIsPriorKey.rawValue as NSString] as? Bool ?? false)
        callback(ourObject, notification)
    }
    
    deinit {
        object?.removeObserver(self, forKeyPath: path, context: nil)
    }
}

extension _KeyValueCodingAndObserving {
    
    ///when the returned NSKeyValueObservation is deinited or invalidated, it will stop observing
    public func observe<Value>(
            _ keyPath: KeyPath<Self, Value>,
            options: NSKeyValueObservingOptions = [],
            changeHandler: @escaping (Self, NSKeyValueObservedChange<Value>) -> Void)
        -> NSKeyValueObservation {
        let result = NSKeyValueObservation(object: self as! NSObject, keyPath: keyPath) { (obj, change) in
            let notification = NSKeyValueObservedChange(kind: change.kind,
                                                        newValue: change.newValue as? Value,
                                                        oldValue: change.oldValue as? Value,
                                                        indexes: change.indexes,
                                                        isPrior: change.isPrior)
            changeHandler(obj as! Self, notification)
        }
        result.start(options)
        return result
    }
    
    public func willChangeValue<Value>(for keyPath: __owned KeyPath<Self, Value>) {
        (self as! NSObject).willChangeValue(forKey: _bridgeKeyPathToString(keyPath))
    }
    
    public func willChange<Value>(_ changeKind: NSKeyValueChange, valuesAt indexes: IndexSet, for keyPath: __owned KeyPath<Self, Value>) {
        (self as! NSObject).willChange(changeKind, valuesAt: indexes, forKey: _bridgeKeyPathToString(keyPath))
    }
    
    public func willChangeValue<Value>(for keyPath: __owned KeyPath<Self, Value>, withSetMutation mutation: NSKeyValueSetMutationKind, using set: Set<Value>) -> Void {
        (self as! NSObject).willChangeValue(forKey: _bridgeKeyPathToString(keyPath), withSetMutation: mutation, using: set)
    }
    
    public func didChangeValue<Value>(for keyPath: __owned KeyPath<Self, Value>) {
        (self as! NSObject).didChangeValue(forKey: _bridgeKeyPathToString(keyPath))
    }
    
    public func didChange<Value>(_ changeKind: NSKeyValueChange, valuesAt indexes: IndexSet, for keyPath: __owned KeyPath<Self, Value>) {
        (self as! NSObject).didChange(changeKind, valuesAt: indexes, forKey: _bridgeKeyPathToString(keyPath))
    }
    
    public func didChangeValue<Value>(for keyPath: __owned KeyPath<Self, Value>, withSetMutation mutation: NSKeyValueSetMutationKind, using set: Set<Value>) -> Void {
        (self as! NSObject).didChangeValue(forKey: _bridgeKeyPathToString(keyPath), withSetMutation: mutation, using: set)
    }
}
