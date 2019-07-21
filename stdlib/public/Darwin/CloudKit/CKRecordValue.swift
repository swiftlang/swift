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

@_exported import CloudKit

// __CKRecordObjCValue - an ObjC object that ObjectiveC clients use as CKRecord values
// CKRecordValueProtocol - a struct or class used only by Swift clients as CKRecord values
// CKRecordValueConvertible - an ObjectiveC object that should be converted to a native Swift type before being used in Swift clients.
// CKObjCRecordValueConvertible - a Swift struct or object that should be converted to a CKRecordObjCValue when set on a CKRecord

/*                      CKRecordValueProtocol    CKObjCRecordValueConvertible
              __CKRecordObjCValue    CKRecordValueConvertible
----------------|-----------|-----------|-----------|-----------
   String       |           |     X     |           |     X
   Date         |           |     X     |           |     X
   Data         |           |     X     |           |     X
   Bool         |           |     X     |           |     X
   Int          |           |     X     |           |     X
   UInt         |           |     X     |           |     X
   Float        |           |     X     |           |     X
   Double       |           |     X     |           |     X
   [U]Int8 et al|           |     X     |           |     X
   Int64        |           |     X     |           |     X
   Array        |           |     X     |           |     X
   NSString     |     X     |     X     |     X     |
   NSDate       |     X     |     X     |     X     |
   NSData       |     X     |     X     |     X     |
   NSNumber     |     X     |     X     |     X     |
   NSArray      |     X     |     X     |     X     |
   CKReference  |     X     |     X     |           |
     aka CKRecord.Reference
   CKAsset      |     X     |     X     |           |
   CLLocation   |     X     |     X     |           |

 */

// MARK: - Protocols for bridging record value types

/// Anything that can be a record value in Swift.
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
public protocol CKRecordValueProtocol {}

@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
public typealias CKRecordValue = __CKRecordObjCValue

// A Swift data type that is convertible to and from a __CKRecordObjCValue
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
private protocol CKObjCRecordValueConvertible : CKRecordValueProtocol {
    // Convert to __CKRecordObjCValue
    func objcRecordValue() -> __CKRecordObjCValue
    // Convert from __CKRecordObjCValue
    static func fromObjcRecordValue(_ objcRecordValue: __CKRecordObjCValue) -> Self
}

// An ObjC data type that is more naturally represented as a native Swift type when pulled from a CKRecord
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
internal protocol CKRecordValueConvertible : __CKRecordObjCValue {
    // Convert to CKRecordValueProtocols
    func swiftNativeValue() -> CKRecordValueProtocol
}

// MARK: - Converting between record value types

@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension __CKRecordObjCValue {
    internal func asSwiftNativeValue() -> CKRecordValueProtocol {
        let swiftValue: CKRecordValueProtocol
        if let convertibleObjcRecordValue = self as? CKRecordValueConvertible {
            swiftValue = convertibleObjcRecordValue.swiftNativeValue()
        } else {
            swiftValue = self as! CKRecordValueProtocol
        }
        return swiftValue
    }
}

@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension CKRecordValueProtocol {
    fileprivate func asObjCRecordValue() -> __CKRecordObjCValue {
        let objcValue: __CKRecordObjCValue
        if let convertibleSwiftRecordValue = self as? CKObjCRecordValueConvertible {
            objcValue = convertibleSwiftRecordValue.objcRecordValue()
        } else {
            objcValue = self as! __CKRecordObjCValue
        }
        return objcValue
    }
}

// MARK: - Get and Set RecordValues on records

@available(macOS 10.11, iOS 9.0, watchOS 3.0, *)
extension CKRecordKeyValueSetting {
    @nonobjc
    public subscript<T:CKRecordValueProtocol>(key: CKRecord.FieldKey) -> T? {
        get {
            if let objcValue: __CKRecordObjCValue = self.object(forKey: key) {
                /* I'd really like to do this statically, by implementing a separate subscript that takes <T:CKObjCRecordValueConvertible>.  But that doesn't seem to work: <rdar://problem/32911583> Can't overload subscript generics with inherited protocols in protocol extension */
                if let ConvertibleT = T.self as? CKObjCRecordValueConvertible.Type {
                    return (ConvertibleT.fromObjcRecordValue(objcValue) as! T)
                } else {
                    return (objcValue.asSwiftNativeValue() as! T)
                }
            }
            return nil
        }
        set {
            self[key] = newValue?.asObjCRecordValue()
        }
    }

    @nonobjc
    public subscript(key: CKRecord.FieldKey) -> CKRecordValueProtocol? {
        get {
            if let objcValue: __CKRecordObjCValue = self.object(forKey: key) {
                return objcValue.asSwiftNativeValue()
            }
            return nil
        }
        set {
            self[key] = newValue?.asObjCRecordValue()
        }
    }
}

// MARK: - CKRecord Value Protocol conformance

@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension CKObjCRecordValueConvertible {
    static func fromObjcRecordValue(_ objcRecordValue: __CKRecordObjCValue) -> Self {
        return objcRecordValue as! Self
    }
}

@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension String : CKRecordValueProtocol, CKObjCRecordValueConvertible {
    fileprivate func objcRecordValue() -> __CKRecordObjCValue { return self as NSString }
}

@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension Date : CKRecordValueProtocol, CKObjCRecordValueConvertible {
    fileprivate func objcRecordValue() -> __CKRecordObjCValue { return self as NSDate }
}

@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension Data : CKRecordValueProtocol, CKObjCRecordValueConvertible {
    fileprivate func objcRecordValue() -> __CKRecordObjCValue { return self as NSData }
}

@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension Bool : CKRecordValueProtocol, CKObjCRecordValueConvertible {
    fileprivate func objcRecordValue() -> __CKRecordObjCValue { return NSNumber(value: self) }
    fileprivate static func fromObjcRecordValue(_ objcRecordValue: __CKRecordObjCValue) -> Bool { return (objcRecordValue as! NSNumber).boolValue }
}
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension Double : CKRecordValueProtocol, CKObjCRecordValueConvertible {
    fileprivate func objcRecordValue() -> __CKRecordObjCValue { return NSNumber(value: self) }
    fileprivate static func fromObjcRecordValue(_ objcRecordValue: __CKRecordObjCValue) -> Double { return (objcRecordValue as! NSNumber).doubleValue }
}
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension Int : CKRecordValueProtocol, CKObjCRecordValueConvertible {
    fileprivate func objcRecordValue() -> __CKRecordObjCValue { return NSNumber(value: self) }
    fileprivate static func fromObjcRecordValue(_ objcRecordValue: __CKRecordObjCValue) -> Int { return (objcRecordValue as! NSNumber).intValue }
}
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension UInt :CKRecordValueProtocol,  CKObjCRecordValueConvertible {
    fileprivate func objcRecordValue() -> __CKRecordObjCValue { return NSNumber(value: self) }
    fileprivate static func fromObjcRecordValue(_ objcRecordValue: __CKRecordObjCValue) -> UInt { return (objcRecordValue as! NSNumber).uintValue }
}

@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension Int8 : CKRecordValueProtocol, CKObjCRecordValueConvertible {
    fileprivate func objcRecordValue() -> __CKRecordObjCValue { return NSNumber(value: self) }
    fileprivate static func fromObjcRecordValue(_ objcRecordValue: __CKRecordObjCValue) -> Int8 { return (objcRecordValue as! NSNumber).int8Value }
}
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension UInt8 : CKRecordValueProtocol, CKObjCRecordValueConvertible {
    fileprivate func objcRecordValue() -> __CKRecordObjCValue { return NSNumber(value: self) }
    fileprivate static func fromObjcRecordValue(_ objcRecordValue: __CKRecordObjCValue) -> UInt8 { return (objcRecordValue as! NSNumber).uint8Value }
}
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension Int16 : CKRecordValueProtocol, CKObjCRecordValueConvertible {
    fileprivate func objcRecordValue() -> __CKRecordObjCValue { return NSNumber(value: self) }
    fileprivate static func fromObjcRecordValue(_ objcRecordValue: __CKRecordObjCValue) -> Int16 { return (objcRecordValue as! NSNumber).int16Value }
}
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension UInt16 : CKRecordValueProtocol, CKObjCRecordValueConvertible {
    fileprivate func objcRecordValue() -> __CKRecordObjCValue { return NSNumber(value: self) }
    fileprivate static func fromObjcRecordValue(_ objcRecordValue: __CKRecordObjCValue) -> UInt16 { return (objcRecordValue as! NSNumber).uint16Value }
}
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension Int32 : CKRecordValueProtocol, CKObjCRecordValueConvertible {
    fileprivate func objcRecordValue() -> __CKRecordObjCValue { return NSNumber(value: self) }
    fileprivate static func fromObjcRecordValue(_ objcRecordValue: __CKRecordObjCValue) -> Int32 { return (objcRecordValue as! NSNumber).int32Value }
}
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension UInt32 : CKRecordValueProtocol, CKObjCRecordValueConvertible {
    fileprivate func objcRecordValue() -> __CKRecordObjCValue { return NSNumber(value: self) }
    fileprivate static func fromObjcRecordValue(_ objcRecordValue: __CKRecordObjCValue) -> UInt32 { return (objcRecordValue as! NSNumber).uint32Value }
}
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension Int64 : CKRecordValueProtocol, CKObjCRecordValueConvertible {
    fileprivate func objcRecordValue() -> __CKRecordObjCValue { return NSNumber(value: self) }
    fileprivate static func fromObjcRecordValue(_ objcRecordValue: __CKRecordObjCValue) -> Int64 { return (objcRecordValue as! NSNumber).int64Value }
}
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension UInt64 : CKRecordValueProtocol, CKObjCRecordValueConvertible {
    fileprivate func objcRecordValue() -> __CKRecordObjCValue { return NSNumber(value: self) }
    fileprivate static func fromObjcRecordValue(_ objcRecordValue: __CKRecordObjCValue) -> UInt64 { return (objcRecordValue as! NSNumber).uint64Value }
}
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension Float : CKRecordValueProtocol, CKObjCRecordValueConvertible {
    fileprivate func objcRecordValue() -> __CKRecordObjCValue { return NSNumber(value: self) }
    fileprivate static func fromObjcRecordValue(_ objcRecordValue: __CKRecordObjCValue) -> Float { return (objcRecordValue as! NSNumber).floatValue }
}


@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension Array : CKRecordValueProtocol, CKObjCRecordValueConvertible where Element: CKRecordValueProtocol  {
    fileprivate func objcRecordValue() -> __CKRecordObjCValue {
        let retVal = NSMutableArray()
        self.forEach { retVal.add($0.asObjCRecordValue()) }
        return retVal
    }
}

@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension NSString : CKRecordValueProtocol, CKRecordValueConvertible {
    internal func swiftNativeValue() -> CKRecordValueProtocol { return self as String }
}
@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension NSDate : CKRecordValueProtocol, CKRecordValueConvertible {
    internal func swiftNativeValue() -> CKRecordValueProtocol { return self as Date }
}
@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension NSData : CKRecordValueProtocol, CKRecordValueConvertible {
    internal func swiftNativeValue() -> CKRecordValueProtocol { return self as Data }
}
@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension NSNumber: CKRecordValueProtocol, CKRecordValueConvertible {
    internal func swiftNativeValue() -> CKRecordValueProtocol {
        // All numbers come back as Int64 or Double, matching our protobuf conversion
        // In some cases (record accessor methods) we have more type info, and can be smarter.  But not here.
        if CFNumberIsFloatType(self) {
            return self.doubleValue
        } else {
            return self.int64Value
        }
    }
}
@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension NSArray : CKRecordValueProtocol, CKRecordValueConvertible {
    internal func swiftNativeValue() -> CKRecordValueProtocol {
        var retVal = [CKRecordValueProtocol]()
        for element in self {
            let objcRecordValue = element as! __CKRecordObjCValue
            let swiftValue = objcRecordValue.asSwiftNativeValue()
            retVal.append(swiftValue)
        }
        return retVal as CKRecordValueProtocol
    }
}

@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension CKRecord.Reference : CKRecordValueProtocol {}
@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension CKAsset : CKRecordValueProtocol {}
@nonobjc
@available(macOS 10.10, iOS 8.0, watchOS 3.0, *)
extension CLLocation : CKRecordValueProtocol {}


