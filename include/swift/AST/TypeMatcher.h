//===--- TypeMatcher.h - Recursive Type Matcher -----------------*- C++ -*-===//
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
// This file defines CanTypeVisitor, a specialized version of
// TypeVisitor for visiting fully type-checked canonical types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_TYPE_MATCHER_H
#define SWIFT_AST_TYPE_MATCHER_H

#include "swift/AST/CanTypeVisitor.h"

namespace swift {

/// Recursively match the structure of two types, calling out those places
/// where the types are not structurally identical and allowing the
/// implementing subclass to take specific actions in response to such
/// mismatches.
///
/// Subclasses must implement the 'mismatch' method, which will be called when
/// a mismatch between the two given types is detected. The most general form
/// of 'mismatch', which all subclasses will likely implement as a catch-all,
/// takes two \c TypeBase pointers:
///
/// \code
/// bool mismatch(TypeBase *firstType, TypeBase *secondType);
/// /\endcode
///
/// \c mismatch will be called with the most specific types. Thus, a client that
/// wants to (say) bind type variables in the first type to types in the second
/// type could isolate that case with a 'mismatch' overload:
///
/// \code
/// bool mismatch(TypeVariableType *firstTypeVar, TypeBase *secondType);
/// \endcode
///
/// While having other mismatches fall to the original case. The 'mismatch'
/// function should return true to indicate that matching should continue,
/// or false to indicate that matching should exit early.
template<typename ImplClass>
class TypeMatcher {
  class MatchVisitor : public CanTypeVisitor<MatchVisitor, bool, Type> {
    TypeMatcher &Matcher;

    /// Dispatch to the derived class when we've destructured the second type.
    template<typename FirstType, typename SecondType>
    bool mismatch(FirstType *firstType, SecondType *secondType) {
      return Matcher.asDerived().mismatch(firstType, secondType);
    }

    /// Dispatch to the most-derived class when we have not yet destructured the
    /// second type.
    template<typename FirstType>
    bool mismatch(FirstType *firstType, Type secondType) {
      switch (secondType->getKind()) {
#define TYPE(CLASS, PARENT)                                         \
      case TypeKind::CLASS:                                         \
        return mismatch(firstType,                                  \
                        cast<CLASS##Type>(secondType.getPointer()));
#include "swift/AST/TypeNodes.def"
      }

      llvm_unreachable("Unhandled TypeKind in switch.");
    }

    /// Honeypot to catch cases where we should redispatch the second type, but
    /// we have a stray ".getPointer()" in the dispatch call.
    template<typename FirstType>
    bool mismatch(FirstType *firstType, TypeBase *secondType) = delete;

  public:
    MatchVisitor(TypeMatcher &matcher) : Matcher(matcher) { }

#define TRIVIAL_CASE(TheType)                                             \
      bool visit##TheType(Can##TheType firstType, Type secondType) {      \
        /* If the types match, continue. */                               \
        if (firstType->isEqual(secondType))                               \
          return true;                                                    \
                                                                          \
        /* Otherwise, let the derived class deal with the mismatch. */    \
        return mismatch(firstType.getPointer(), secondType);              \
      }

    TRIVIAL_CASE(ErrorType)
    TRIVIAL_CASE(BuiltinIntegerType)
    TRIVIAL_CASE(BuiltinFloatType)
    TRIVIAL_CASE(BuiltinRawPointerType)
    TRIVIAL_CASE(BuiltinNativeObjectType)
    TRIVIAL_CASE(BuiltinBridgeObjectType)
    TRIVIAL_CASE(BuiltinUnknownObjectType)
    TRIVIAL_CASE(BuiltinUnsafeValueBufferType)
    TRIVIAL_CASE(BuiltinVectorType)

    bool visitTupleType(CanTupleType firstTuple, Type secondType) {
      if (auto secondTuple = secondType->getAs<TupleType>()) {
        if (firstTuple->getNumElements() != secondTuple->getNumElements())
          return mismatch(firstTuple.getPointer(), secondTuple);

        for (unsigned i = 0, n = firstTuple->getNumElements(); i != n; ++i) {
          const auto &firstElt = firstTuple->getElements()[i];
          const auto &secondElt = secondTuple->getElements()[i];

          if (firstElt.getName() != secondElt.getName() ||
              firstElt.isVararg() != secondElt.isVararg())
            return mismatch(firstTuple.getPointer(), secondTuple);

          // Recurse on the tuple elements.
          if (!this->visit(firstTuple.getElementType(i), secondElt.getType()))
            return false;
        }

        return true;
      }

      // Tuple/non-tuple mismatch.
      return mismatch(firstTuple.getPointer(), secondType);
    }

    template<typename FirstReferenceStorageType>
    bool handleReferenceStorageType(
           CanTypeWrapper<FirstReferenceStorageType> firstStorage,
           Type secondType) {
      if (auto secondStorage = secondType->getAs<FirstReferenceStorageType>()) {
        return this->visit(firstStorage.getReferentType(),
                           secondStorage->getReferentType());
      }

      return mismatch(firstStorage.getPointer(), secondType);
    }

    bool visitUnownedStorageType(CanUnownedStorageType firstStorage,
                                 Type secondType) {
      return handleReferenceStorageType(firstStorage, secondType);
    }

    bool visitUnmanagedStorageType(CanUnmanagedStorageType firstStorage,
                                   Type secondType) {
      return handleReferenceStorageType(firstStorage, secondType);
    }

    bool visitWeakStorageType(CanWeakStorageType firstStorage,
                              Type secondType) {
      return handleReferenceStorageType(firstStorage, secondType);
    }

    template<typename FirstNominalType>
    bool handleNominalType(CanTypeWrapper<FirstNominalType> firstNominal,
                           Type secondType) {
      if (auto secondNominal = secondType->getAs<FirstNominalType>()) {
        if (firstNominal->getDecl() != secondNominal->getDecl())
          return mismatch(firstNominal.getPointer(), secondNominal);

        if (firstNominal.getParent())
          return this->visit(firstNominal.getParent(),
                             secondNominal->getParent());

        return true;
      }

      return mismatch(firstNominal.getPointer(), secondType);
    }

    bool visitEnumType(CanEnumType firstEnum, Type secondType) {
      return handleNominalType(firstEnum, secondType);
    }

    bool visitStructType(CanStructType firstStruct, Type secondType) {
      return handleNominalType(firstStruct, secondType);
    }

    bool visitClassType(CanClassType firstClass, Type secondType) {
      return handleNominalType(firstClass, secondType);
    }

    bool visitProtocolType(CanProtocolType firstProtocol, Type secondType) {
      return handleNominalType(firstProtocol, secondType);
    }

    template<typename FirstMetatypeType>
    bool handleAnyMetatypeType(CanTypeWrapper<FirstMetatypeType> firstMeta,
                               Type secondType) {
      if (auto secondMeta = secondType->getAs<FirstMetatypeType>()) {
        if (firstMeta->getKind() != secondMeta->getKind())
          return mismatch(firstMeta.getPointer(), secondMeta);

        return this->visit(firstMeta.getInstanceType(),
                           secondMeta->getInstanceType());
      }

      return mismatch(firstMeta.getPointer(), secondType);
    }

    bool visitMetatypeType(CanMetatypeType firstMeta, Type secondType) {
      return handleAnyMetatypeType(firstMeta, secondType);
    }

    bool visitExistentialMetatypeType(CanExistentialMetatypeType firstMeta,
                                      Type secondType) {
      return handleAnyMetatypeType(firstMeta, secondType);
    }

    TRIVIAL_CASE(ModuleType)
    TRIVIAL_CASE(DynamicSelfType)
    TRIVIAL_CASE(ArchetypeType)
    TRIVIAL_CASE(GenericTypeParamType)

    bool visitDependentMemberType(CanDependentMemberType firstDepMember,
                                  Type secondType) {
      if (auto secondDepMember = secondType->getAs<DependentMemberType>()) {
        if (firstDepMember->getAssocType() != secondDepMember->getAssocType() ||
            firstDepMember->getName() != secondDepMember->getName())
          return mismatch(firstDepMember.getPointer(), secondDepMember);

        return this->visit(firstDepMember.getBase(),
                           secondDepMember->getBase());
      }

      return mismatch(firstDepMember.getPointer(), secondType);
    }

    /// FIXME: Split this out into cases?
    bool visitAnyFunctionType(CanAnyFunctionType firstFunc, Type secondType) {
      if (auto secondFunc = secondType->getAs<AnyFunctionType>()) {
        // FIXME: Compare throws()? Both existing subclasses would prefer
        // to mismatch on (!firstFunc->throws() && secondFunc->throws()), but
        // embedding that non-commutativity in this general matcher is icky.
        if (firstFunc->isNoEscape() != secondFunc->isNoEscape())
          return mismatch(firstFunc.getPointer(), secondFunc);
        
        return this->visit(firstFunc.getInput(), secondFunc->getInput()) &&
               this->visit(firstFunc.getResult(), secondFunc->getResult());
      }

      return mismatch(firstFunc.getPointer(), secondType);
    }

    TRIVIAL_CASE(SILFunctionType)
    TRIVIAL_CASE(SILBlockStorageType)
    TRIVIAL_CASE(SILBoxType)
    TRIVIAL_CASE(ProtocolCompositionType)

    bool visitLValueType(CanLValueType firstLValue, Type secondType) {
      if (auto secondLValue = secondType->getAs<LValueType>()) {
        return this->visit(firstLValue.getObjectType(),
                           secondLValue->getObjectType());
      }

      return mismatch(firstLValue.getPointer(), secondType);
    }

    bool visitInOutType(CanInOutType firstInOut, Type secondType) {
      if (auto secondInOut = secondType->getAs<InOutType>()) {
        return this->visit(firstInOut.getObjectType(),
                           secondInOut->getObjectType());
      }

      return mismatch(firstInOut.getPointer(), secondType);
    }

    bool visitUnboundBoundGenericType(CanUnboundGenericType firstUBGT,
                                      Type secondType) {
      if (auto secondUBGT = secondType->getAs<UnboundGenericType>()) {
        if (firstUBGT->getDecl() != secondUBGT->getDecl())
          return mismatch(firstUBGT.getPointer(), secondUBGT);

        if (firstUBGT.getParent())
          return this->visit(firstUBGT.getParent(), secondUBGT->getParent());

        return true;
      }

      return mismatch(firstUBGT.getPointer(), secondType);
    }

    template<typename FirstBoundGenericType>
    bool handleBoundGenericType(CanTypeWrapper<FirstBoundGenericType> firstBGT,
                                Type secondType) {
      if (auto secondBGT = secondType->getAs<FirstBoundGenericType>()) {
        if (firstBGT->getDecl() != secondBGT->getDecl())
          return mismatch(firstBGT.getPointer(), secondBGT);

        if (firstBGT->getParent() &&
            !this->visit(firstBGT.getParent(), secondBGT->getParent()))
          return false;

        for (unsigned i = 0, n = firstBGT->getGenericArgs().size(); i != n; ++i) {
          if (!this->visit(firstBGT.getGenericArgs()[i],
                           secondBGT->getGenericArgs()[i]))
            return false;
        }

        return true;
      }

      return mismatch(firstBGT.getPointer(), secondType);
    }

    bool visitBoundGenericClassType(CanBoundGenericClassType firstBGT,
                                    Type secondType) {
      return handleBoundGenericType(firstBGT, secondType);
    }

    bool visitBoundGenericEnumType(CanBoundGenericEnumType firstBGT,
                                   Type secondType) {
      return handleBoundGenericType(firstBGT, secondType);
    }

    bool visitBoundGenericStructType(CanBoundGenericStructType firstBGT,
                                     Type secondType) {
      return handleBoundGenericType(firstBGT, secondType);
    }

    TRIVIAL_CASE(TypeVariableType)

#undef TRIVIAL_CASE
  };

  ImplClass &asDerived() { return static_cast<ImplClass &>(*this); }

  const ImplClass &asDerived() const {
    return static_cast<const ImplClass &>(*this);
  }

public:
  bool match(Type first, Type second) {
    return MatchVisitor(*this).visit(first->getCanonicalType(), second);
  }
};

} // end namespace swift

#endif // SWIFT_AST_TYPE_MATCHER_H
