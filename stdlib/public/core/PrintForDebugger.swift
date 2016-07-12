//===--- PrintForDebugger.swift -------------------------------------------===//
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

public enum _PrintForDebugger {

  enum CollectionStatus {
    case NotACollection
    case CollectionOfElements
    case CollectionOfPairs
    case Element
    case Pair
    case ElementOfPair
  
    var isCollection: Bool {
      return self != .NotACollection
    }
  
    func getChildStatus(child: Mirror) -> CollectionStatus {
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

  static func asObjectIdentifier(value: Any) -> ObjectIdentifier? {
      if let ao = value as? AnyObject {
          return ObjectIdentifier(ao)
      } else {
          return nil
      }
  }

  static func asNumericValue(value: Any) -> Int {
      if let ao = value as? AnyObject {
          return unsafeBitCast(ao, to: Int.self)
      } else {
          return 0
      }
  }

  static func asStringRepresentation(value: Any?,
                              mirror: Mirror,
                              count: Int) -> String? {
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
        return "\(Int(mirror.children.count)) elements"
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
            return "<\(x.dynamicType): 0x\(String(asNumericValue(value: x), radix: 16, uppercase: false))>"
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

  static func ivarCount(mirror: Mirror) -> Int {
    let count = Int(mirror.children.count)
    if let sc = mirror.superclassMirror {
      return ivarCount(mirror: sc) + count
    } else {
      return count
    }
  }


  static func shouldExpand(mirror: Mirror,
                    collectionStatus: CollectionStatus,
                    isRoot: Bool) -> Bool {
    if isRoot || collectionStatus.isCollection { return true }
    let count = Int(mirror.children.count)
    if count > 0 { return true }
    if let sc = mirror.superclassMirror {
      return ivarCount(mirror: sc) > 0
    } else {
      return true
    }
  }

  static func printForDebuggerImpl<StreamType: OutputStream>(
    value: Any?,
    mirror: Mirror,
    name: String?,
    indent: Int,
    maxDepth: Int,
    isRoot: Bool,
    parentCollectionStatus: CollectionStatus,
    refsAlreadySeen: inout Set<ObjectIdentifier>,
    maxItemCounter: inout Int,
    targetStream: inout StreamType) {
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
    let bullet = isRoot && (count == 0 || willExpand == false) ? ""
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
      if let valueIdentifier = asObjectIdentifier(value: x) {
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
      printForDebuggerImpl(value: nil,
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
    
      printForDebuggerImpl(value: child,
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

  struct StringOutput: OutputStream {
    var data = ""
    mutating func _lock() {
    }

    mutating func _unlock() {
    }

    mutating func write(_ string: String) {
        data += string
    }
  }

  public static func printForDebugger(value: Any) -> String {
    var maxItemCounter = Int.max
    var refs = Set<ObjectIdentifier>()
    var targetStream = StringOutput()

    printForDebuggerImpl(value: value,
                         mirror: Mirror(reflecting: value),
                         name: nil,
                         indent: 0,
                         maxDepth: maxItemCounter,
                         isRoot: true,
                         parentCollectionStatus: .NotACollection,
                         refsAlreadySeen: &refs,
                         maxItemCounter: &maxItemCounter,
                         targetStream: &targetStream)

    return targetStream.data
  }
}

