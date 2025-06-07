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

/// Stores the variance positions at which a type references a specific
/// generic parameter.
class GenericParameterReferenceInfo final {
  static constexpr unsigned NumTypePositionBits = 4;
  static_assert(NumTypePositions <= NumTypePositionBits,
                "Not enough bits to store all cases");

  uint8_t DirectRefs : NumTypePositionBits;
  uint8_t DepMemberTyRefs : NumTypePositionBits;

  /// Whether there is a reference to the generic parameter at hand in covariant
  /// result type position. This position is the uncurried interface type of a
  /// declaration, stipped of any optionality. For example, this is true for
  /// 'Self' in 'func foo(Int) -> () -> Self?'.
  bool HasCovariantGenericParamResult;

  GenericParameterReferenceInfo(uint8_t DirectRefs, uint8_t DepMemberTyRefs,
                                bool HasCovariantGenericParamResult)
      : DirectRefs(DirectRefs), DepMemberTyRefs(DepMemberTyRefs),
        HasCovariantGenericParamResult(HasCovariantGenericParamResult) {}

public:
  GenericParameterReferenceInfo()
      : GenericParameterReferenceInfo(0, 0, false) {}

  /// A direct reference to the generic parameter.
  static GenericParameterReferenceInfo forDirectRef(TypePosition pos) {
    return GenericParameterReferenceInfo(pos, 0, false);
  }

  /// A direct reference to the generic parameter in covariant result type
  /// position.
  static GenericParameterReferenceInfo forCovariantGenericParamResult() {
    return GenericParameterReferenceInfo(TypePosition::Covariant, 0, true);
  }

  /// A reference to a dependent member type rooted on the generic parameter.
  static GenericParameterReferenceInfo
  forDependentMemberTypeRef(TypePosition pos) {
    return GenericParameterReferenceInfo(0, pos, false);
  }

  bool hasDirectRef(std::optional<TypePosition> pos = std::nullopt) const {
    if (!pos) {
      return DirectRefs;
    }

    return DirectRefs & pos.value();
  }

  bool hasDependentMemberTypeRef(
      std::optional<TypePosition> pos = std::nullopt) const {
    if (!pos) {
      return DepMemberTyRefs;
    }

    return DepMemberTyRefs & pos.value();
  }

  bool hasRef(std::optional<TypePosition> pos = std::nullopt) const {
    return hasDirectRef(pos) || hasDependentMemberTypeRef(pos);
  }

  bool hasNonCovariantRef() const {
    const uint8_t notCovariant = ~TypePosition::Covariant;
    return (DirectRefs & notCovariant) || (DepMemberTyRefs & notCovariant);
  }

  bool hasCovariantGenericParamResult() const {
    return HasCovariantGenericParamResult;
  }

  GenericParameterReferenceInfo &
  operator|=(const GenericParameterReferenceInfo &other);

  explicit operator bool() const { return hasRef(); }
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

/// Describes the limitation on accessing a protocol member on a value of
/// existential type.
enum class ExistentialMemberAccessLimitation : uint8_t {
  /// The member can be freely accessed on the existential.
  None,
  /// The storage member is available only for reads.
  ReadOnly,
  /// The storage member is available only for writes.
  WriteOnly,
  /// Accessing the member on the existential is not supported.
  Unsupported,
};

/// Compute the limitations on accessing the given member on a value of the
/// given existential base type.
ExistentialMemberAccessLimitation
isMemberAvailableOnExistential(Type baseTy, const ValueDecl *member);

/// Determine whether opening an existential argument for a function parameter
/// is supported.
/// A necessary condition for this is that the parameter interface type contains
/// a generic parameter type to which the opened argument can bind.
///
/// \param callee The function or subscript being called.
/// \param paramIdx The index specifying which function parameter is being
/// initialized.
/// \param paramTy The type of the parameter as it was opened in the constraint
/// system.
/// \param argTy The type of the argument.
///
/// \returns If opening is supported, returns the type variable representing the
/// generic parameter type, and the unopened type it binds to.
std::optional<std::pair<TypeVariableType *, Type>>
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
