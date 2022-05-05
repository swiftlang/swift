//===--- Value.swift - the Value protocol ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Basic
import SILBridging

public protocol Value : AnyObject, CustomStringConvertible {
  var uses: UseList { get }
  var type: Type { get }
  var definingInstruction: Instruction? { get }
}

extension Value {
  public var description: String {
    var s = SILNode_debugDescription(bridgedNode)
    return String(cString: s.c_str())
  }

  public var uses: UseList {
    UseList(SILValue_firstUse(bridged))
  }
  
  public var function: Function {
    SILNode_getFunction(bridgedNode).function
  }

  public var type: Type { SILValue_getType(bridged).type }

  public var definingInstruction: Instruction? {
    switch self {
      case let inst as SingleValueInstruction:
        return inst
      case let mvi as MultipleValueInstructionResult:
        return mvi.instruction
      default:
        return nil
    }
  }

  public var definingBlock: BasicBlock? {
    if let inst = definingInstruction {
      return inst.block
    }
    if let arg = self as? Argument {
      return arg.block
    }
    return nil
  }

  public var hashable: HashableValue { ObjectIdentifier(self) }

  public var bridged: BridgedValue {
    BridgedValue(obj: SwiftObject(self as AnyObject))
  }
  var bridgedNode: BridgedNode {
    BridgedNode(obj: SwiftObject(self as AnyObject))
  }
}

public typealias HashableValue = ObjectIdentifier

// We can't make `Value` inherit from `Equatable`, since `Equatable` is a PAT,
// and we do use `Value` existentials around the codebase. Thus functions from
// `Equatable` are declared separately.
public func ==(_ lhs: Value, _ rhs: Value) -> Bool {
  return lhs === rhs
}

public func !=(_ lhs: Value, _ rhs: Value) -> Bool {
  return !(lhs === rhs)
}

extension BridgedValue {
  public func getAs<T: AnyObject>(_ valueType: T.Type) -> T { obj.getAs(T.self) }

  public var value: Value {
    // This is much faster than a conformance lookup with `as! Value`.
    let v = getAs(AnyObject.self)
    switch v {
      case let inst as SingleValueInstruction:
        return inst
      case let arg as Argument:
        return arg
      case let mvr as MultipleValueInstructionResult:
        return mvr
      case let undef as Undef:
        return undef
      default:
        fatalError("unknown Value type")
    }
  }
}

final class Undef : Value {
  public var definingInstruction: Instruction? { nil }
}

final class PlaceholderValue : Value {
  public var definingInstruction: Instruction? { nil }
}

extension OptionalBridgedValue {
  var value: Value? { obj.getAs(AnyObject.self) as? Value }
}
