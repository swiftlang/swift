//===--- GlobalVariable.swift - Defines the GlobalVariable class ----------===//
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

final public class GlobalVariable : CustomStringConvertible, HasShortDescription, Hashable {
  public var varDecl: VarDecl? {
    VarDecl(bridged: bridged.getDecl())
  }

  public var name: StringRef {
    return StringRef(bridged: bridged.getName())
  }

  public var description: String {
    return String(taking: bridged.getDebugDescription())
  }

  public var shortDescription: String { name.string }

  public var isLet: Bool { bridged.isLet() }

  /// True, if the linkage of the global variable indicates that it is visible outside the current
  /// compilation unit and therefore not all of its uses are known.
  ///
  /// For example, `public` linkage.
  public var isPossiblyUsedExternally: Bool {
    return bridged.isPossiblyUsedExternally()
  }

  /// True, if the linkage of the global indicates that it has a definition outside the
  /// current compilation unit.
  ///
  /// For example, `public_external` linkage.
  public var isAvailableExternally: Bool {
    return bridged.isAvailableExternally()
  }

  public var staticInitializerInstructions: InstructionList? {
    if let firstStaticInitInst = bridged.getFirstStaticInitInst().instruction {
      return InstructionList(first: firstStaticInitInst)
    }
    return nil
  }

  public var staticInitValue: SingleValueInstruction? {
    if let staticInitInsts = staticInitializerInstructions {
      return staticInitInsts.reversed().first! as? SingleValueInstruction
    }
    return nil
  }

  /// True if the global's linkage and resilience expansion allow the global
  /// to be initialized statically.
  public var canBeInitializedStatically: Bool {
    return bridged.canBeInitializedStatically()
  }

  public var mustBeInitializedStatically: Bool {
    return bridged.mustBeInitializedStatically()
  }

  public static func ==(lhs: GlobalVariable, rhs: GlobalVariable) -> Bool {
    lhs === rhs
  }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(self))
  }

  public var bridged: BridgedGlobalVar { BridgedGlobalVar(SwiftObject(self)) }
}

// Bridging utilities

extension BridgedGlobalVar {
  public var globalVar: GlobalVariable { obj.getAs(GlobalVariable.self) }

  public var optional: OptionalBridgedGlobalVar {
    OptionalBridgedGlobalVar(obj: self.obj)
  }
}

extension OptionalBridgedGlobalVar {
  public var globalVar: GlobalVariable? { obj.getAs(GlobalVariable.self) }

  public static var none: OptionalBridgedGlobalVar {
    OptionalBridgedGlobalVar(obj: nil)
  }
}
