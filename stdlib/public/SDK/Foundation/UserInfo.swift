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

internal class _NSSwiftUserInfoItem : NSObject {
    internal var value: Any
    internal init(value: Any) {
        self.value = value
    }
}

internal class _NSUserInfoDictionaryKeyEnumerator : NSEnumerator {
    private var _objcKeyGenerator: LazyMapIterator<DictionaryIterator<NSObject, AnyObject>, NSObject>?
    private var _swiftKeyGenerator: LazyMapIterator<DictionaryIterator<NSObject, Any>, NSObject>?
    
    internal init(userInfo: _NSUserInfoDictionary) {
        _objcKeyGenerator = userInfo._objc.keys.makeIterator()
        _swiftKeyGenerator = userInfo._swift.keys.makeIterator()
    }
    
    @objc
    internal override func nextObject() -> AnyObject? {
        if let next = _objcKeyGenerator?.next() {
            return next
        } else {
            _objcKeyGenerator = nil
        }
        if let next = _swiftKeyGenerator?.next() {
            return next
        } else {
            _swiftKeyGenerator = nil
        }
        return nil
    }
    
}

internal class _NSUserInfoDictionary : NSMutableDictionary {
    private var _objc = [NSObject : AnyObject]()
    private var _swift = [NSObject : Any]()
    
    /// Note: these two init methods are the only proper method of construction and must be marked as "convenience" and call super.init() because the overlay has required init methods in extensions
    private convenience init(objc: [NSObject : AnyObject], swift: [NSObject : Any]) {
        self.init()
        _objc = objc
        _swift = swift
    }
    
    internal convenience init(dictionary dict: NSDictionary) {
        self.init()
        dict.enumerateKeysAndObjects(options: []) { k, v, stop in
            _objc[k as! NSObject] = v
        }
    }

    @objc
    internal override var count: Int {
        return _objc.count + _swift.count
    }
    
    @objc
    internal override func object(forKey aKey: AnyObject) -> AnyObject? {
        if let key = aKey as? String {
            if let val = _objc[key as NSString] {
                return val
            } else if let val = _swift[key as NSString] {
                return _NSSwiftUserInfoItem(value: val)
            }
        }
        return nil
    }
    
    @objc
    internal override func keyEnumerator() -> NSEnumerator {
        return _NSUserInfoDictionaryKeyEnumerator(userInfo: self)
    }
    
    @objc
    internal override func copy(with zone: NSZone?) -> AnyObject {
        return self
    }
    
    @objc
    internal override func mutableCopy(with zone: NSZone?) -> AnyObject {
        return _NSUserInfoDictionary(objc: _objc, swift: _swift)
    }
    
    @objc
    internal override func removeObject(forKey key: AnyObject) {
        if let k = key as? String {
            _objc[k as NSString] = nil
            _swift[k as NSString] = nil
        }
    }
    
    @objc
    internal override func setObject(_ anObject: AnyObject, forKey aKey: NSCopying) {
        if let key = aKey as? String {
            if let obj = anObject as? NSObject {
                if let box = obj as? _NSSwiftUserInfoItem {
                    _swift[key as NSString] = box.value
                } else {
                    _objc[key as NSString] = obj
                }
            }
        }
    }

    internal var dictionary: [NSObject : Any] {
        var result = [NSObject : Any]()
        for (k, v) in _objc {
            result[k] = v
        }
        for (k, v) in _swift {
            result[k] = v
        }
        return result
    }
    
    internal static func compare<KeyType : Hashable>(_ lhs: [KeyType : Any], _ rhs: [KeyType : Any]) -> Bool {
        let lhsCount = lhs.count
        let rhsCount = rhs.count
        if lhsCount == 0 && rhsCount == 0 {
            return true
        }
        if lhsCount != rhsCount {
            return false
        }
        
        for (lhsKey, lhsValue) in lhs {
            if let rhsValue = rhs[lhsKey] {
                if let lhsObj = lhsValue as? NSObject {
                    if let rhsObj = rhsValue as? NSObject {
                        if lhsObj.isEqual(rhsObj) {
                            continue
                        }
                    }
                }
            }
            return false
        }
        return true
    }

    internal static func bridgeValue(from source: [String : Any]) -> AnyObject {
        var objc = [NSObject : AnyObject]()
        var swift = [NSObject : Any]()
        for (k, v) in source {
            if let val = v as? _NSSwiftUserInfoItem {
                swift[k._bridgeToObjectiveC()] = val.value
            } else if let obj = v as? NSObject {
                objc[k._bridgeToObjectiveC()] = obj
            } else {
                swift[k._bridgeToObjectiveC()] = v
            }
        }

        return _NSUserInfoDictionary(objc: objc, swift: swift)
    }

    internal static func bridgeValue(from source: [NSObject : Any]) -> AnyObject {
        var objc = [NSObject : AnyObject]()
        var swift = [NSObject : Any]()
        for (k, v) in source {
            if let val = v as? _NSSwiftUserInfoItem {
                swift[k] = val.value
            } else if let obj = v as? NSObject {
                objc[k] = obj
            } else {
                swift[k] = v
            }
        }

        return _NSUserInfoDictionary(objc: objc, swift: swift)
    }

    internal static func bridgeReference<KeyType : Hashable>(from source: AnyObject) -> [KeyType : Any]? {
        if let info = source as? _NSUserInfoDictionary {
            var result = [KeyType : Any]()
            for (k, v) in info._objc {
                if let key = k as? KeyType {
                    result[key] = v
                } else {
                    return nil
                }
            }
            for (k, v) in info._swift {
                if let key = k as? KeyType {
                    result[key] = v
                }
            }
            return result
        } else if let info = source as? NSDictionary {
            var result = [KeyType : Any]()
            var success = true
            info.enumerateKeysAndObjects(options: []) { k, v, stop in
                if let key = k as? KeyType {
                    result[key] = v
                } else {
                    success = false
                    stop.pointee = true
                }
            }
            if !success {
                return nil
            }
            return result
        } else {
            return nil
        }
    }
}
