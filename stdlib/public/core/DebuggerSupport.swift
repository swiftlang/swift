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

// Macros are disabled when Swift is built without swift-syntax.
#if $Macros && hasAttribute(attached)

/// Converts description definitions to a debugger Type Summary.
///
/// This macro converts compatible description implementations written in Swift
/// to an LLDB format known as a Type Summary. A Type Summary is LLDB's
/// equivalent to `debugDescription`, with the distinction that it does not
/// execute code inside the debugged process. By avoiding code execution,
/// descriptions can be produced faster, without potential side effects, and
/// shown in situations where code execution is not performed, such as the
/// variable list of an IDE.
///
/// Consider this an example. This `Team` struct has a `debugDescription` which
/// summarizes some key details, such as the team's name. The debugger only
/// computes this string on demand - typically via the `po` command. By applying
/// the `DebugDescription` macro, a matching Type Summary is constructed. This
/// allows the user to show a string like "Rams [11-2]", without executing
/// `debugDescription`. This improves the usability, performance, and
/// reliability of the debugging experience.
///
///     @DebugDescription
///     struct Team: CustomDebugStringConvertible {
///        var name: String
///        var wins, losses: Int
///
///        var debugDescription: String {
///            "\(name) [\(wins)-\(losses)]"
///        }
///     }
///
/// The `DebugDescription` macro supports both `debugDescription`, `description`,
/// as well as a third option: a property named `lldbDescription`. The first
/// two are implemented when conforming to the `CustomDebugStringConvertible`
/// and `CustomStringConvertible` protocols. The additional `lldbDescription`
/// property is useful when both `debugDescription` and `description` are
/// implemented, but don't meet the requirements of the `DebugDescription`
/// macro. If `lldbDescription` is implemented, `DebugDescription` choose it
/// over `debugDescription` and `description`. Likewise, `debugDescription` is
/// preferred over `description`.
///
/// ### Description Requirements
///
/// The description implementation has the following requirements:
///
/// * The body of the description implementation must a single string
///   expression. String concatenation is not supported, use string interpolation
///   instead.
/// * String interpolation can reference stored properties only, functions calls
///   and other arbitrary computation are not supported. Of note, conditional
///   logic and computed properties are not supported.
/// * Overloaded string interpolation cannot be used.
@attached(member)
@attached(memberAttribute)
public macro DebugDescription() =
  #externalMacro(module: "SwiftMacros", type: "DebugDescriptionMacro")

/// Internal-only macro. See `@DebugDescription`.
@attached(peer, names: named(_lldb_summary))
public macro _DebugDescriptionProperty(_ debugIdentifier: String, _ computedProperties: [String]) =
  #externalMacro(module: "SwiftMacros", type: "_DebugDescriptionPropertyMacro")

#endif

#if SWIFT_ENABLE_REFLECTION

@frozen // namespace
public enum _DebuggerSupport {
  private enum CollectionStatus {
    case notACollection
    case collectionOfElements
    case collectionOfPairs
    case element
    case pair
    case elementOfPair
  
    internal var isCollection: Bool {
      return self != .notACollection
    }
  
    internal func getChildStatus(child: Mirror) -> CollectionStatus {
      let disposition = child.displayStyle
    
      if disposition == .collection { return .collectionOfElements }
      if disposition == .dictionary { return .collectionOfPairs }
      if disposition == .set { return .collectionOfElements }
    
      if self == .collectionOfElements { return .element }
      if self == .collectionOfPairs { return .pair }
      if self == .pair { return .elementOfPair }
    
      return .notACollection
    }
  }

  private static func isClass(_ value: Any) -> Bool {
    return type(of: value) is AnyClass
  }
  
  private static func checkValue<T>(
    _ value: Any,
    ifClass: (AnyObject) -> T,
    otherwise: () -> T
  ) -> T {
    if isClass(value) {
      return ifClass(_unsafeDowncastToAnyObject(fromAny: value))
    }
    return otherwise()
  }

  private static func asObjectIdentifier(_ value: Any) -> ObjectIdentifier? {
    return checkValue(value,
      ifClass: { return ObjectIdentifier($0) },
      otherwise: { return nil })
  }

  private static func asObjectAddress(_ value: Any) -> String {
    let address = checkValue(value,
      ifClass: { return unsafe unsafeBitCast($0, to: Int.self) },
      otherwise: { return 0 })
    return String(address, radix: 16, uppercase: false)
  }

  private static func asStringRepresentation(
    value: Any?,
    mirror: Mirror,
    count: Int
  ) -> String? {
    switch mirror.displayStyle {
    case .optional? where count > 0:
        return "\(mirror.subjectType)"
    case .optional?:
      return value.map(String.init(reflecting:))
    case .collection?, .dictionary?, .set?, .tuple?:
      return count == 1 ? "1 element" : "\(count) elements"
    case .`struct`?, .`enum`?, nil:
      switch value {
      case let x as CustomDebugStringConvertible:
        return x.debugDescription
      case let x as CustomStringConvertible:
        return x.description
      case _ where count > 0:
        return "\(mirror.subjectType)"
      default:
        return value.map(String.init(reflecting:))
      }
    case .`class`?, .foreignReference?:
      switch value {
      case let x as CustomDebugStringConvertible:
        return x.debugDescription
      case let x as CustomStringConvertible:
        return x.description
      case let x?:
        // for a Class with no custom summary, mimic the Foundation default
        return "<\(type(of: x)): 0x\(asObjectAddress(x))>"
      default:
        // but if I can't provide a value, just use the type anyway
        return "\(mirror.subjectType)"
      }
    }
  }

  private static func ivarCount(mirror: Mirror) -> Int {
    let ivars = mirror.superclassMirror.map(ivarCount) ?? 0
    return ivars + mirror.children.count
  }

  private static func shouldExpand(
    mirror: Mirror,
    collectionStatus: CollectionStatus,
    isRoot: Bool
  ) -> Bool {
    if isRoot || collectionStatus.isCollection { return true }
    if !mirror.children.isEmpty { return true }
    if mirror.displayStyle == .`class` { return true }
    if let sc = mirror.superclassMirror { return ivarCount(mirror: sc) > 0 }
    return true
  }

  private static func printForDebuggerImpl<StreamType: TextOutputStream>(
    value: Any?,
    mirror: Mirror,
    name: String?,
    indent: Int,
    maxDepth: Int,
    isRoot: Bool,
    parentCollectionStatus: CollectionStatus,
    refsAlreadySeen: inout Set<ObjectIdentifier>,
    maxItemCounter: inout Int,
    target: inout StreamType
  ) {    
    guard maxItemCounter > 0 else { return }

    guard shouldExpand(mirror: mirror,
                       collectionStatus: parentCollectionStatus,
                       isRoot: isRoot) 
    else { return }

    maxItemCounter -= 1
  
    print(String(repeating: " ", count: indent), terminator: "", to: &target)

    // do not expand classes with no custom Mirror
    // yes, a type can lie and say it's a class when it's not since we only
    // check the displayStyle - but then the type would have a custom Mirror
    // anyway, so there's that...
    let isNonClass = mirror.displayStyle != .`class`
    let isCustomReflectable: Bool
    if let value = value {
      isCustomReflectable = value is CustomReflectable
    } else {
      isCustomReflectable = true
    }
    let willExpand = isNonClass || isCustomReflectable

    let count = mirror.children.count
    let bullet = isRoot && (count == 0 || !willExpand) ? ""
      : count == 0    ? "- "
      : maxDepth <= 0 ? "▹ " : "▿ "
    print(bullet, terminator: "", to: &target)
  
    let collectionStatus = parentCollectionStatus.getChildStatus(child: mirror)
  
    if let name = name {
      print("\(name) : ", terminator: "", to: &target)
    }

    if isRoot, let value = value as? String {
      // We don't want to use string's debug desciprtion for 'po' because it
      // escapes the string and prints it raw (e.g. prints "\n" instead of
      // actually printing a newline), but only if its the root value. Otherwise,
      // continue using the debug description.
      print(value, terminator: "", to: &target)
    } else if let str = asStringRepresentation(value: value, mirror: mirror, count: count) {
      print(str, terminator: "", to: &target)
    }
  
    if (maxDepth <= 0) || !willExpand {
      print("", to: &target)
      return
    }

    if let valueIdentifier = value.flatMap(asObjectIdentifier) {
      if refsAlreadySeen.contains(valueIdentifier) {
        print(" { ... }", to: &target)
        return
      } else {
        refsAlreadySeen.insert(valueIdentifier)
      }
    }

    print("", to: &target)
  
    var printedElements = 0
  
    if let sc = mirror.superclassMirror {
      printForDebuggerImpl(
        value: nil,
        mirror: sc,
        name: "super",
        indent: indent + 2,
        maxDepth: maxDepth - 1,
        isRoot: false,
        parentCollectionStatus: .notACollection,
        refsAlreadySeen: &refsAlreadySeen,
        maxItemCounter: &maxItemCounter,
        target: &target)
    }
  
    for (optionalName,child) in mirror.children {
      let childName = optionalName ?? "\(printedElements)"
      if maxItemCounter <= 0 {
        print(String(repeating: " ", count: indent+4), terminator: "", to: &target)
        let remainder = count - printedElements
        print("(\(remainder)", terminator: "", to: &target)
        if printedElements > 0 {
          print(" more", terminator: "", to: &target)
        }
        print(remainder == 1 ? " child)" : " children)", to: &target)
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
        target: &target)
      printedElements += 1
    }
  }

  public static func stringForPrintObject(_ value: Any) -> String {
    var maxItemCounter = Int.max
    var refs = Set<ObjectIdentifier>()
    var target = ""

    printForDebuggerImpl(
      value: value,
      mirror: Mirror(reflecting: value),
      name: nil,
      indent: 0,
      maxDepth: maxItemCounter,
      isRoot: true,
      parentCollectionStatus: .notACollection,
      refsAlreadySeen: &refs,
      maxItemCounter: &maxItemCounter,
      target: &target)

    return target
  }
}

public func _stringForPrintObject(_ value: Any) -> String {
  return _DebuggerSupport.stringForPrintObject(value)
}

#endif // SWIFT_ENABLE_REFLECTION

public func _debuggerTestingCheckExpect(_: String, _: String) { }

@_alwaysEmitIntoClient @_transparent
internal func _withHeapObject<R>(
  of object: AnyObject,
  _ body: (UnsafeMutableRawPointer) -> R
) -> R {
  defer { _fixLifetime(object) }
  let unmanaged = unsafe Unmanaged.passUnretained(object)
  return unsafe body(unmanaged.toOpaque())
}

@_extern(c, "swift_retainCount") @usableFromInline
internal func _swift_retainCount(_: UnsafeMutableRawPointer) -> Int
@_extern(c, "swift_unownedRetainCount") @usableFromInline
internal func _swift_unownedRetainCount(_: UnsafeMutableRawPointer) -> Int
@_extern(c, "swift_weakRetainCount") @usableFromInline
internal func _swift_weakRetainCount(_: UnsafeMutableRawPointer) -> Int

// Utilities to get refcount(s) of class objects.
@_alwaysEmitIntoClient
public func _getRetainCount(_ object: AnyObject) -> UInt {
  let count = unsafe _withHeapObject(of: object) { unsafe _swift_retainCount($0) }
  return UInt(bitPattern: count)
}

@_alwaysEmitIntoClient
public func _getUnownedRetainCount(_ object: AnyObject) -> UInt {
  let count = unsafe _withHeapObject(of: object) { unsafe _swift_unownedRetainCount($0) }
  return UInt(bitPattern: count)
}

@_alwaysEmitIntoClient
public func _getWeakRetainCount(_ object: AnyObject) -> UInt {
  let count = unsafe _withHeapObject(of: object) { unsafe _swift_weakRetainCount($0) }
  return UInt(bitPattern: count)
}
