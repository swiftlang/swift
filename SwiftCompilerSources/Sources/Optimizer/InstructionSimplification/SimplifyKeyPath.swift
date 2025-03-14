//===--- SimplifyKeyPath.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL
import OptimizerBridging

extension KeyPathInst : OnoneSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if allUsesRemovable(instruction: self) {
      removeAllUses(context)
    } else {
      trySpecialize(self, context)
    }
  }

  func simplifyOnone(_ context: SimplifyContext) {
    if allUsesRemovable(instruction: self) {
      removeAllUses(context)
    }
  }
}

extension KeyPathInst {
  fileprivate func removeAllUses(_ context: SimplifyContext) {
    if parentFunction.hasOwnership {
      let builder = Builder(after: self, context)
      for operand in self.operands {
        if !operand.value.type.isTrivial(in: parentFunction) {
          if operand.value.type.isAddress {
            builder.createDestroyAddr(address: operand.value)
          } else {
            builder.createDestroyValue(operand: operand.value)
          }
        }
      }
    }
    context.erase(instructionIncludingAllUsers: self)
  }
}

fileprivate func allUsesRemovable(instruction: Instruction) -> Bool {
  for result in instruction.results {
    for use in result.uses {
      switch use.instruction {
      case is UpcastInst,
           is DestroyValueInst,
           is StrongReleaseInst,
           is BeginBorrowInst,
           is EndBorrowInst,
           is MoveValueInst,
           is CopyValueInst:
        // This is a removable instruction type, continue descending into uses
        if !allUsesRemovable(instruction: use.instruction) {
          return false
        }

      default:
        return false
      }
    }
  }
  return true
}

fileprivate func trySpecialize(_ inst: KeyPathInst, _ context: SimplifyContext) {
  let subs = inst.substitutions

  // If we don't have a substitution map, then this isn't a keypath that is
  // specializable to begin with.
  guard !subs.isEmpty else {
    return
  }

  guard let pattern = inst.pattern else {
    return
  }

  for component in pattern.components {
    if !component.subscriptIndices.isEmpty {
      return
    }
  }

  let kpTy = inst.keyPathType

  guard let genericArgs = kpTy.genericArguments else {
    return
  }

  let rootTy = genericArgs[0].canonical
  let valueTy = genericArgs[1].canonical

  // Both 'Root' and 'Value' must be completely concrete.
  if rootTy.hasArchetype || valueTy.hasArchetype {
    return
  }

  var specializedComponents: [KeyPathPattern.Component] = []
  specializedComponents.reserveCapacity(pattern.components.count)

  for component in pattern.components {
    let specializedComponentTy = component.componentType.subst(with: subs)

    switch component.kind {
    case .gettableProperty,
         .settableProperty:
      let getter = component.computedPropertyGetter
      guard let specializedGetter = specializeKeyPathAccessor(
        context,
        getter,
        subs
      ) else {
        return
      }

      if component.kind == .gettableProperty {
        let component = KeyPathPattern.Component.forComputedGettableProperty(
          component.computedPropertyId,
          specializedGetter,
          component.subscriptIndices,
          component.subscriptIndexEquals,
          component.subscriptIndexHash,
          component.externalDecl,
          component.externalSubstitutions,
          specializedComponentTy
        )

        specializedComponents.append(component)
        continue
      }

      let setter = component.computedPropertySetter
      guard let specializedSetter = specializeKeyPathAccessor(
        context,
        setter,
        subs
      ) else {
        return
      }

      let component = KeyPathPattern.Component.forComputedSettableProperty(
        component.computedPropertyId,
        specializedGetter,
        specializedSetter,
        component.subscriptIndices,
        component.subscriptIndexEquals,
        component.subscriptIndexHash,
        component.externalDecl,
        component.externalSubstitutions,
        specializedComponentTy
      )

      specializedComponents.append(component)

    case .storedProperty:
      let component = KeyPathPattern.Component.forStoredProperty(
        component.storedProperty,
        specializedComponentTy
      )

      specializedComponents.append(component)

    case .tupleElement:
      let component = KeyPathPattern.Component.forTupleElement(
        component.tupleIndex,
        specializedComponentTy
      )

      specializedComponents.append(component)

    case .optionalChain,
         .optionalForce,
         .optionalWrap:
      let component = KeyPathPattern.Component.forOptional(
        component.kind,
        specializedComponentTy
      )

      specializedComponents.append(component)
    }
  }

  let specializedPattern = KeyPathPattern(
    inst.parentFunction,
    rootTy,
    valueTy,
    specializedComponents,
    pattern.objcString
  )

  var values: [Value] = []
  values.reserveCapacity(inst.operands.count)

  for operand in inst.operands {
    values.append(operand.value)
  }

  let builder = Builder(after: inst, context)
  let newKp = builder.createKeyPath(
    pattern: specializedPattern,
    values: values,
    keyPathType: inst.type
  )

  inst.replace(with: newKp, context)
}

fileprivate func specializeKeyPathAccessor(
  _ context: SimplifyContext,
  _ accessor: Function,
  _ substitutions: SubstitutionMap
) -> Function? {
  BridgedKeyPathPattern_trySpecializeAccessor(
    context._bridged,
    accessor.bridged,
    substitutions.bridged
  ).function
}
