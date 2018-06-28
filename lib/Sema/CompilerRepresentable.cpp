//===--- CompilerRepresentable.cpp - Check compiler evaluability of types -===//
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
//
// SWIFT_ENABLE_TENSORFLOW
// Checks if the compile-time evaluator can handle values of a type.
//
//===----------------------------------------------------------------------===//

#include "CompilerRepresentable.h"
#include "swift/AST/ParameterList.h"

namespace swift {

UnrepresentableReasonPtr
UnrepresentableReason::getIsClass(Type UnrepresentableType) {
  return std::unique_ptr<UnrepresentableReason>(
      new UnrepresentableReason(IsClass, UnrepresentableType, nullptr));
}

UnrepresentableReasonPtr
UnrepresentableReason::getUnknownTypeKind(Type UnrepresentableType) {
  return std::unique_ptr<UnrepresentableReason>(
      new UnrepresentableReason(UnknownTypeKind, UnrepresentableType, nullptr));
}

UnrepresentableReasonPtr UnrepresentableReason::getMetatypeOfUnrepresentable(
    Type UnrepresentableType, UnrepresentableReasonPtr UnderlyingReason) {
  assert(UnderlyingReason);
  return std::unique_ptr<UnrepresentableReason>(
      new UnrepresentableReason(MetatypeOfUnrepresentable, UnrepresentableType,
                                std::move(UnderlyingReason)));
}

UnrepresentableReasonPtr UnrepresentableReason::getGenericArgUnrepresentable(
    Type UnrepresentableType, UnrepresentableReasonPtr UnderlyingReason) {
  assert(UnderlyingReason);
  return std::unique_ptr<UnrepresentableReason>(
      new UnrepresentableReason(GenericArgUnrepresentable, UnrepresentableType,
                                std::move(UnderlyingReason)));
}

UnrepresentableReasonPtr UnrepresentableReason::getEnumElementUnrepresentable(
    Type UnrepresentableType,
    const EnumElementDecl *UnrepresentableEnumElementDecl,
    UnrepresentableReasonPtr UnderlyingReason) {
  assert(UnderlyingReason);
  auto reason = std::unique_ptr<UnrepresentableReason>(
      new UnrepresentableReason(EnumElementUnrepresentable, UnrepresentableType,
                                std::move(UnderlyingReason)));
  reason->UnrepresentableEnumElementDecl = UnrepresentableEnumElementDecl;
  return reason;
}

UnrepresentableReasonPtr UnrepresentableReason::getFieldUnrepresentable(
    Type UnrepresentableType, const VarDecl *UnrepresentableFieldDecl,
    UnrepresentableReasonPtr UnderlyingReason) {
  assert(UnderlyingReason);
  auto reason = std::unique_ptr<UnrepresentableReason>(
      new UnrepresentableReason(FieldUnrepresentable, UnrepresentableType,
                                std::move(UnderlyingReason)));
  reason->UnrepresentableFieldDecl = UnrepresentableFieldDecl;
  return reason;
}

UnrepresentableReasonPtr UnrepresentableReason::getTupleElementUnrepresentable(
    Type UnrepresentableType, UnrepresentableReasonPtr UnderlyingReason) {
  assert(UnderlyingReason);
  return std::unique_ptr<UnrepresentableReason>(new UnrepresentableReason(
      TupleElementUnrepresentable, UnrepresentableType,
      std::move(UnderlyingReason)));
}

UnrepresentableReasonPtr UnrepresentableReason::getParamUnrepresentable(
    Type UnrepresentableType, UnrepresentableReasonPtr UnderlyingReason) {
  assert(UnderlyingReason);
  return std::unique_ptr<UnrepresentableReason>(new UnrepresentableReason(
      ParamUnrepresentable, UnrepresentableType, std::move(UnderlyingReason)));
}

UnrepresentableReasonPtr UnrepresentableReason::getResultUnrepresentable(
    Type UnrepresentableType, UnrepresentableReasonPtr UnderlyingReason) {
  assert(UnderlyingReason);
  return std::unique_ptr<UnrepresentableReason>(new UnrepresentableReason(
      ResultUnrepresentable, UnrepresentableType, std::move(UnderlyingReason)));
}

template <class DiagTarget>
void UnrepresentableReason::emitDiagnosticNotesImpl(TypeChecker &TC,
                                                    DiagTarget Target) const {
  // TODO(marcrasi): Make the notes point at the problematic decls rather than
  // the places where they get used.

  switch (Kind) {
  case IsClass:
    TC.diagnose(Target, diag::compiler_evaluable_unrepresentable_class,
                UnrepresentableType);
    return;
  case UnknownTypeKind:
    return;
  case MetatypeOfUnrepresentable:
    assert(UnderlyingReason);
    TC.diagnose(Target, diag::compiler_evaluable_unrepresentable_metatype,
                UnrepresentableType, UnderlyingReason->UnrepresentableType);
    break;
  case GenericArgUnrepresentable:
    assert(UnderlyingReason);
    TC.diagnose(Target, diag::compiler_evaluable_unrepresentable_generic_arg,
                UnrepresentableType, UnderlyingReason->UnrepresentableType);
    break;
  case EnumElementUnrepresentable:
    assert(UnderlyingReason);

    if (UnrepresentableFieldDecl->getDeclContext()->isChildContextOf(
            TC.Context.TheStdlibModule)) {
      TC.diagnose(Target, diag::compiler_evaluable_unrepresentable_stdlib_type,
                  UnrepresentableType);
      return;
    }

    TC.diagnose(Target, diag::compiler_evaluable_unrepresentable_enum_element,
                UnrepresentableType, UnrepresentableEnumElementDecl->getName(),
                UnderlyingReason->UnrepresentableType);
    break;
  case FieldUnrepresentable:
    assert(UnderlyingReason);

    if (UnrepresentableFieldDecl->getDeclContext()->isChildContextOf(
            TC.Context.TheStdlibModule)) {
      TC.diagnose(Target, diag::compiler_evaluable_unrepresentable_stdlib_type,
                  UnrepresentableType);
      return;
    }

    TC.diagnose(Target, diag::compiler_evaluable_unrepresentable_field,
                UnrepresentableType, UnrepresentableFieldDecl->getName(),
                UnderlyingReason->UnrepresentableType);
    break;
  case TupleElementUnrepresentable:
    assert(UnderlyingReason);
    TC.diagnose(Target, diag::compiler_evaluable_unrepresentable_tuple_element,
                UnrepresentableType, UnderlyingReason->UnrepresentableType);
    break;
  case ParamUnrepresentable:
    assert(UnderlyingReason);
    TC.diagnose(Target, diag::compiler_evaluable_unrepresentable_param,
                UnrepresentableType, UnderlyingReason->UnrepresentableType);
    break;
  case ResultUnrepresentable:
    assert(UnderlyingReason);
    TC.diagnose(Target, diag::compiler_evaluable_unrepresentable_result,
                UnrepresentableType, UnderlyingReason->UnrepresentableType);
    break;
  }

  assert(UnderlyingReason);
  UnderlyingReason->emitDiagnosticNotesImpl(TC, Target);
}

void UnrepresentableReason::emitDiagnosticNotes(TypeChecker &TC,
                                                const Decl *Target) const {
  emitDiagnosticNotesImpl(TC, Target);
}

void UnrepresentableReason::emitDiagnosticNotes(TypeChecker &TC,
                                                SourceLoc Target) const {
  emitDiagnosticNotesImpl(TC, Target);
}

// Checks if the compile-time evaluator can handle the types that the generic
// arguments are bound to.
UnrepresentableReasonPtr
CompilerRepresentableChecker::checkGenericArgs(CanBoundGenericType type) {
  assert(type);
  for (auto arg : type->getGenericArgs()) {
    if (auto unrepresentable = check(arg)) {
      return UnrepresentableReason::getGenericArgUnrepresentable(
          type, std::move(unrepresentable));
    }
  }
  return nullptr;
}

// Checks if the compile-time evaluator can handle this enum.
UnrepresentableReasonPtr
CompilerRepresentableChecker::checkDeclaredType(const EnumDecl *decl) {
  assert(decl);
  for (auto *elementDecl : decl->getAllElements()) {
    auto *parameterList = elementDecl->getParameterList();
    if (!parameterList)
      continue;

    for (auto *paramDecl : *parameterList) {
      if (auto unrepresentable = check(paramDecl->getType())) {
        return UnrepresentableReason::getEnumElementUnrepresentable(
            decl->getDeclaredType(), elementDecl, std::move(unrepresentable));
      }
    }
  }
  return nullptr;
}

// Checks if the compile-time evaluator can handle this struct.
UnrepresentableReasonPtr
CompilerRepresentableChecker::checkDeclaredType(const StructDecl *decl) {
  assert(decl);
  for (auto *fieldDecl : decl->getStoredProperties()) {
    if (auto unrepresentable = check(fieldDecl->getType())) {
      return UnrepresentableReason::getFieldUnrepresentable(
          decl->getDeclaredType(), fieldDecl, std::move(unrepresentable));
    }
  }
  return nullptr;
}

// Checks if the compile-time evaluator can handle the types of the parameters
// of this function.
UnrepresentableReasonPtr
CompilerRepresentableChecker::checkParams(CanAnyFunctionType type) {
  assert(type);
  for (auto param : type->getParams()) {
    if (auto unrepresentable = check(param.getType())) {
      return UnrepresentableReason::getParamUnrepresentable(
          type, std::move(unrepresentable));
    }
  }
  return nullptr;
}

// Checks if the compile-time evaluator can handle values of type `type`.
UnrepresentableReasonPtr CompilerRepresentableChecker::check(Type type) {
  if (!type)
    return nullptr;

  auto canType = type->getCanonicalType();
  if (isa<BuiltinIntegerType>(canType)) {
    // Allowed builtins.
    return nullptr;
  } else if (isa<ArchetypeType>(canType) ||
             isa<GenericTypeParamType>(canType) ||
             isa<UnboundGenericType>(canType)) {
    // Always allow generic types, because we always check that the concrete
    // types that they get bound to are allowed (see the BoundGeneric* cases).
    return nullptr;
  } else if (isa<ErrorType>(canType)) {
    // Allow ErrorTypes so that we don't pile additional errors on top of them.
    return nullptr;
  } else if (isa<ClassType>(canType) || isa<BoundGenericClassType>(canType)) {
    return UnrepresentableReason::getIsClass(canType);
  } else if (auto enumType = dyn_cast<EnumType>(canType)) {
    return checkDeclaredType(enumType->getDecl());
  } else if (auto enumType = dyn_cast<BoundGenericEnumType>(canType)) {
    if (auto unrepresentable = checkDeclaredType(enumType->getDecl())) {
      return unrepresentable;
    }

    // When the decl check that we just did visits a generic type, it accepts it
    // regardless of what it is bound to, because we do not tell it what any of
    // the generic types are bound to. So now we need to make sure that all the
    // generic types are bound to compiler-representable types. This is overly
    // conservative.
    return checkGenericArgs(enumType);
  } else if (auto structType = dyn_cast<StructType>(canType)) {
    return checkDeclaredType(structType->getDecl());
  } else if (auto structType = dyn_cast<BoundGenericStructType>(canType)) {
    if (auto unrepresentable = checkDeclaredType(structType->getDecl())) {
      return unrepresentable;
    }

    // Same thing as in the BoundGenericEnumType case. See the comment there.
    return checkGenericArgs(structType);
  } else if (auto anyFunctionType = dyn_cast<AnyFunctionType>(canType)) {
    if (auto unrepresentable = checkParams(anyFunctionType)) {
      return unrepresentable;
    }
    if (auto unrepresentable = check(anyFunctionType.getResult())) {
      return UnrepresentableReason::getResultUnrepresentable(
          canType, std::move(unrepresentable));
    }
    return nullptr;
  } else if (auto metatypeType = dyn_cast<MetatypeType>(canType)) {
    if (auto unrepresentable = check(metatypeType->getInstanceType())) {
      return UnrepresentableReason::getMetatypeOfUnrepresentable(
          canType, std::move(unrepresentable));
    }
    return nullptr;
  } else if (auto tupleType = dyn_cast<TupleType>(canType)) {
    for (auto element : tupleType->getElements()) {
      if (auto unrepresentable = check(element.getType())) {
        return UnrepresentableReason::getTupleElementUnrepresentable(
            canType, std::move(unrepresentable));
      }
    }
    return nullptr;
  } else if (auto lValueType = dyn_cast<LValueType>(canType)) {
    return check(lValueType.getObjectType());
  } else if (auto inOutType = dyn_cast<InOutType>(canType)) {
    return check(inOutType.getObjectType());
  } else {
    return UnrepresentableReason::getUnknownTypeKind(canType);
  }
}

} // end namespace swift
