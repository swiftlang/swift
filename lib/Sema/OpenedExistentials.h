//===--- OpenedExistentials.h - Utilities for existential types -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_OPENED_EXISTENTIALS_H
#define SWIFT_OPENED_EXISTENTIALS_H

#include "swift/AST/Type.h"
#include "swift/Basic/OptionalEnum.h"
#include <optional>

namespace swift {

class CanGenericSignature;
class GenericTypeParamType;

/// Describes the least favorable positions at which a requirement refers
/// to a given generic parameter in terms of variance, for use in the
/// is-inheritable and is-available-existential checks.
class GenericParameterReferenceInfo final {
  using OptionalTypePosition = OptionalEnum<decltype(TypePosition::Covariant)>;

public:
  /// Whether the uncurried interface type of the declaration, stripped of any
  /// optionality, is a direct reference to the generic parameter at hand. For
  /// example, "func foo(x: Int) -> () -> Self?" has a covariant 'Self' result.
  bool hasCovariantSelfResult;

  OptionalTypePosition selfRef;
  OptionalTypePosition assocTypeRef;

  /// A reference to 'Self'.
  static GenericParameterReferenceInfo forSelfRef(TypePosition position) {
    return GenericParameterReferenceInfo(false, position, std::nullopt);
  }

  /// A reference to the generic parameter in covariant result position.
  static GenericParameterReferenceInfo forCovariantResult() {
    return GenericParameterReferenceInfo(true, TypePosition::Covariant,
                                         std::nullopt);
  }

  /// A reference to 'Self' through an associated type.
  static GenericParameterReferenceInfo forAssocTypeRef(TypePosition position) {
    return GenericParameterReferenceInfo(false, std::nullopt, position);
  }

  GenericParameterReferenceInfo &operator|=(const GenericParameterReferenceInfo &other);

  explicit operator bool() const {
    return hasCovariantSelfResult || selfRef || assocTypeRef;
  }

  GenericParameterReferenceInfo()
      : hasCovariantSelfResult(false), selfRef(std::nullopt),
        assocTypeRef(std::nullopt) {}

private:
  GenericParameterReferenceInfo(bool hasCovariantSelfResult, OptionalTypePosition selfRef,
                    OptionalTypePosition assocTypeRef)
      : hasCovariantSelfResult(hasCovariantSelfResult), selfRef(selfRef),
        assocTypeRef(assocTypeRef) {}
};

/// Find references to the given generic parameter in the type signature of the
/// given declaration using the given generic signature.
///
/// \param skipParamIndex If the value is a function or subscript declaration,
/// specifies the index of the parameter that shall be skipped.
GenericParameterReferenceInfo
findGenericParameterReferences(const ValueDecl *value, CanGenericSignature sig,
                               GenericTypeParamType *origParam,
                               GenericTypeParamType *openedParam,
                               std::optional<unsigned> skipParamIndex);

/// Find references to 'Self' in the type signature of this declaration.
GenericParameterReferenceInfo findExistentialSelfReferences(const ValueDecl *value);

/// Determine whether referencing the given member on the
/// given existential base type is supported. This is the case only if the
/// type of the member, spelled in the context of \p baseTy, does not contain
/// 'Self' or 'Self'-rooted dependent member types in non-covariant position.
bool isMemberAvailableOnExistential(Type baseTy,
                                    const ValueDecl *member);

/// Flags that should be applied to the existential argument type after
/// opening.
enum class OpenedExistentialAdjustmentFlags {
  /// The argument should be made inout after opening.
  InOut = 0x01,
  LValue = 0x02,
};

using OpenedExistentialAdjustments =
  OptionSet<OpenedExistentialAdjustmentFlags>;

/// Determine whether we should open up the existential argument to the
/// given parameters.
///
/// \param callee The function or subscript being called.
/// \param paramIdx The index specifying which function parameter is being
/// initialized.
/// \param paramTy The type of the parameter as it was opened in the constraint
/// system.
/// \param argTy The type of the argument.
///
/// \returns If the argument type is existential and opening it can bind a
/// generic parameter in the callee, returns the generic parameter, type
/// variable (from the opened parameter type) the existential type that needs
/// to be opened (from the argument type), and the adjustments that need to be
/// applied to the existential type after it is opened.
std::optional<std::tuple<GenericTypeParamType *, TypeVariableType *,
                                Type, OpenedExistentialAdjustments>>
canOpenExistentialCallArgument(ValueDecl *callee, unsigned paramIdx,
                               Type paramTy, Type argTy);

/// Given a type that includes an existential type that has been opened to
/// the given type variable, replace the opened type variable and its member
/// types with their upper bounds.
Type typeEraseOpenedExistentialReference(Type type, Type existentialBaseType,
                                         TypeVariableType *openedTypeVar,
                                         TypePosition outermostPosition);


/// Given a type that includes opened existential archetypes derived from
/// the given generic environment, replace the archetypes with their upper
/// bounds.
Type typeEraseOpenedArchetypesFromEnvironment(Type type,
                                              GenericEnvironment *env);

} // end namespace swift

#endif