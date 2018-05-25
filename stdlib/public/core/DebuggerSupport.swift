//===--- DebuggerSupport.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

@_frozen // FIXME(sil-serialize-all)
public enum _DebuggerSupport {
  @_frozen // FIXME(sil-serialize-all)
  @usableFromInline // FIXME(sil-serialize-all)
  internal enum CollectionStatus {
    case NotACollection
    case CollectionOfElements
    case CollectionOfPairs
    case Element
    case Pair
    case ElementOfPair
  
    @inlinable // FIXME(sil-serialize-all)
    internal var isCollection: Bool {
      return self != .NotACollection
    }
  
    @inlinable // FIXME(sil-serialize-all)
    internal func getChildStatus(child: Mirror) -> CollectionStatus {
      let disposition = child.displayStyle ?? .struct
    
      if disposition == .collection { return .CollectionOfElements }
      if disposition == .dictionary { return .CollectionOfPairs }
      if disposition == .set { return .CollectionOfElements }
    
      if self == .CollectionOfElements { return .Element }
      if self == .CollectionOfPairs { return .Pair }
      if self == .Pair { return .ElementOfPair }
    
      return .NotACollection
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal static func isClass(_ value: Any) -> Bool {
    if let _ = type(of: value) as? AnyClass {
      return true
    }
    return false
  }
  
  @inlinable // FIXME(sil-serialize-all)
  internal static func checkValue<T>(
    _ value: Any,
    ifClass: (AnyObject) -> T,
    otherwise: () -> T
  ) -> T {
    if isClass(value) {
      return ifClass(_unsafeDowncastToAnyObject(fromAny: value))
    }
    return otherwise()
  }

  @inlinable // FIXME(sil-serialize-all)
  internal static func asObjectIdentifier(_ value: Any) -> ObjectIdentifier? {
    return checkValue(value,
      ifClass: { return ObjectIdentifier($0) },
      otherwise: { return nil })
  }

  @inlinable // FIXME(sil-serialize-all)
  internal static func asNumericValue(_ value: Any) -> Int {
    return checkValue(value,
      ifClass: { return unsafeBitCast($0, to: Int.self) },
      otherwise: { return 0 })
  }

  @inlinable // FIXME(sil-serialize-all)
  internal static func asStringRepresentation(
    value: Any?,
    mirror: Mirror,
    count: Int
  ) -> String? {
    let ds = mirror.displayStyle ?? .`struct`
    switch ds {
      case .optional:
        if count > 0 {
          return "\(mirror.subjectType)"
        }
        else {
          if let x = value {
            return String(reflecting: x)
          }
        }
      case .collection:
        fallthrough
      case .dictionary:
        fallthrough
      case .set:
        fallthrough
      case .tuple:
        if count == 1 {
          return "1 element"
        } else {
          return "\(count) elements"
        }
      case .`struct`:
        fallthrough
      case .`enum`:
        if let x = value {
          if let cdsc = (x as? CustomDebugStringConvertible) {
            return cdsc.debugDescription
          }
          if let csc = (x as? CustomStringConvertible) {
            return csc.description
          }
        }
        if count > 0 {
            return "\(mirror.subjectType)"
        }
      case .`class`:
        if let x = value {
          if let cdsc = (x as? CustomDebugStringConvertible) {
            return cdsc.debugDescription
          }
          if let csc = (x as? CustomStringConvertible) {
            return csc.description
          }
          // for a Class with no custom summary, mimic the Foundation default
          return "<\(type(of: x)): 0x\(String(asNumericValue(x), radix: 16, uppercase: false))>"
        } else {
          // but if I can't provide a value, just use the type anyway
          return "\(mirror.subjectType)"
        }
    }
    if let x = value {
      return String(reflecting: x)
    }
    return nil
  }

  @inlinable // FIXME(sil-serialize-all)
  internal static func ivarCount(mirror: Mirror) -> Int {
    let count = Int(mirror.children.count)
    if let sc = mirror.superclassMirror {
      return ivarCount(mirror: sc) + count
    } else {
      return count
    }
  }


  @inlinable // FIXME(sil-serialize-all)
  internal static func shouldExpand(
    mirror: Mirror,
    collectionStatus: CollectionStatus,
    isRoot: Bool
  ) -> Bool {
    if isRoot || collectionStatus.isCollection { return true }
    let count = Int(mirror.children.count)
    if count > 0 { return true }
    if let ds = mirror.displayStyle {
      if ds == .`class` {
        return true
      }
    }
    if let sc = mirror.superclassMirror {
      return ivarCount(mirror: sc) > 0
    } else {
      return true
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal static func printForDebuggerImpl<StreamType : TextOutputStream>(
    value: Any?,
    mirror: Mirror,
    name: String?,
    indent: Int,
    maxDepth: Int,
    isRoot: Bool,
    parentCollectionStatus: CollectionStatus,
    refsAlreadySeen: inout Set<ObjectIdentifier>,
    maxItemCounter: inout Int,
    targetStream: inout StreamType
  ) {
    if maxItemCounter <= 0 {
      return
    }

    if !shouldExpand(mirror: mirror,
                     collectionStatus: parentCollectionStatus,
                     isRoot: isRoot) {
      return
    }

    maxItemCounter -= 1
  
    for _ in 0..<indent {
      print(" ", terminator: "", to: &targetStream)
    }

    // do not expand classes with no custom Mirror
    // yes, a type can lie and say it's a class when it's not since we only
    // check the displayStyle - but then the type would have a custom Mirror
    // anyway, so there's that...
    var willExpand = true
    if let ds = mirror.displayStyle {
      if ds == .`class` {
        if let x = value {
          if !(x is CustomReflectable) {
            willExpand = false
          }
        }
      }
    }

    let count = Int(mirror.children.count)
    let bullet = isRoot && (count == 0 || !willExpand) ? ""
      : count == 0    ? "- "
      : maxDepth <= 0 ? "▹ " : "▿ "
    print("\(bullet)", terminator: "", to: &targetStream)
  
    let collectionStatus = parentCollectionStatus.getChildStatus(child: mirror)
  
    if let nam = name {
      print("\(nam) : ", terminator: "", to: &targetStream)
    }

    if let str = asStringRepresentation(value: value, mirror: mirror, count: count) {
      print("\(str)", terminator: "", to: &targetStream)
    }
  
    if (maxDepth <= 0) || !willExpand {
      print("", to: &targetStream)
      return
    }

    if let x = value {
      if let valueIdentifier = asObjectIdentifier(x) {
        if refsAlreadySeen.contains(valueIdentifier) {
          print(" { ... }", to: &targetStream)
          return
        } else {
          refsAlreadySeen.insert(valueIdentifier)
        }
      }
    }

    print("", to: &targetStream)
  
    var printedElements = 0
  
    if let sc = mirror.superclassMirror {
      printForDebuggerImpl(
        value: nil,
        mirror: sc,
        name: "super",
        indent: indent + 2,
        maxDepth: maxDepth - 1,
        isRoot: false,
        parentCollectionStatus: .NotACollection,
        refsAlreadySeen: &refsAlreadySeen,
        maxItemCounter: &maxItemCounter,
        targetStream: &targetStream)
    }
  
    for (optionalName,child) in mirror.children {
      let childName = optionalName ?? "\(printedElements)"
      if maxItemCounter <= 0 {
        for _ in 0..<(indent+4) {
          print(" ", terminator: "", to: &targetStream)
        }
        let remainder = count - printedElements
        print("(\(remainder)", terminator: "", to: &targetStream)
        if printedElements > 0 {
          print(" more", terminator: "", to: &targetStream)
        }
        if remainder == 1 {
          print(" child)", to: &targetStream)
        } else {
          print(" children)", to: &targetStream)
        }
        return
      }
    
      printForDebuggerImpl(
        value: child,
        mirror: Mirror(reflecting: child),
        name: childName,
        indent: indent + 2,
        maxDepth: maxDepth - 1,
        isRoot: false,
        parentCollectionStatus: collectionStatus,
        refsAlreadySeen: &refsAlreadySeen,
        maxItemCounter: &maxItemCounter,
        targetStream: &targetStream)
      printedElements += 1
    }
  }

  // LLDB uses this function in expressions, and if it is inlined the resulting
  // LLVM IR is enormous.  As a result, to improve LLDB performance we are not
  // making it @inlinable.
  public static func stringForPrintObject(_ value: Any) -> String {
    var maxItemCounter = Int.max
    var refs = Set<ObjectIdentifier>()
    var targetStream = ""

    printForDebuggerImpl(
      value: value,
      mirror: Mirror(reflecting: value),
      name: nil,
      indent: 0,
      maxDepth: maxItemCounter,
      isRoot: true,
      parentCollectionStatus: .NotACollection,
      refsAlreadySeen: &refs,
      maxItemCounter: &maxItemCounter,
      targetStream: &targetStream)

    return targetStream
  }
}

@inline(never)
public
func _stringForPrintObject(_ value: Any) -> String {
  return _DebuggerSupport.stringForPrintObject(value)
}

@inline(never)
public
func _debuggerTestingCheckExpect(_ checked_value: String,
                                 _ expected_value: String) {}

// Utilities to get refcount(s) of class objects.
@_silgen_name("swift_retainCount")
public func _getRetainCount(_ Value: AnyObject) -> UInt
@_silgen_name("swift_unownedRetainCount")
public func _getUnownedRetainCount(_ Value : AnyObject) -> UInt
@_silgen_name("swift_weakRetainCount")
public func _getWeakRetainCount(_ Value : AnyObject) -> UInt
