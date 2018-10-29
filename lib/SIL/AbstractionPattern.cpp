//===--- AbstractionPattern.cpp - Abstraction patterns --------------------===//
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
// This file defines routines relating to abstraction patterns.
// working in concert with the Clang importer.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "libsil"
#include "swift/SIL/TypeLowering.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/PrettyPrinter.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

using namespace swift;
using namespace swift::Lowering;

AbstractionPattern
TypeConverter::getAbstractionPattern(AbstractStorageDecl *decl,
                                     bool isNonObjC) {
  if (auto var = dyn_cast<VarDecl>(decl)) {
    return getAbstractionPattern(var, isNonObjC);
  } else {
    return getAbstractionPattern(cast<SubscriptDecl>(decl), isNonObjC);
  }
}

AbstractionPattern
TypeConverter::getAbstractionPattern(SubscriptDecl *decl, bool isNonObjC) {
  CanGenericSignature genericSig;
  if (auto sig = decl->getGenericSignatureOfContext())
    genericSig = sig->getCanonicalSignature();
  return AbstractionPattern(genericSig,
                            decl->getElementInterfaceType()
                                ->getCanonicalType());
}

static const clang::Type *getClangType(const clang::Decl *decl) {
  if (auto valueDecl = dyn_cast<clang::ValueDecl>(decl)) {
    return valueDecl->getType().getTypePtr();
  }

  // This should *really* be a ValueDecl.
  return cast<clang::ObjCPropertyDecl>(decl)->getType().getTypePtr();
}

AbstractionPattern
TypeConverter::getAbstractionPattern(VarDecl *var, bool isNonObjC) {
  CanGenericSignature genericSig;
  if (auto sig = var->getDeclContext()->getGenericSignatureOfContext())
    genericSig = sig->getCanonicalSignature();

  CanType swiftType = var->getInterfaceType()
                         ->getCanonicalType();

  if (isNonObjC)
    return AbstractionPattern(genericSig, swiftType);

  if (auto clangDecl = var->getClangDecl()) {
    auto clangType = getClangType(clangDecl);
    auto contextType = var->getDeclContext()->mapTypeIntoContext(swiftType);
    swiftType = getLoweredBridgedType(
        AbstractionPattern(genericSig, swiftType, clangType),
        contextType,
        SILFunctionTypeRepresentation::CFunctionPointer,
        TypeConverter::ForMemory)->getCanonicalType();
    return AbstractionPattern(genericSig, swiftType, clangType);
  }

  return AbstractionPattern(genericSig, swiftType);
}

AbstractionPattern TypeConverter::getAbstractionPattern(EnumElementDecl *decl) {
  assert(decl->hasAssociatedValues());
  assert(!decl->hasClangNode());

  // This cannot be implemented correctly for Optional.Some.
  assert(!decl->getParentEnum()->isOptionalDecl() &&
         "Optional.Some does not have a unique abstraction pattern because "
         "optionals are re-abstracted");

  CanGenericSignature genericSig;
  if (auto sig = decl->getParentEnum()->getGenericSignatureOfContext())
    genericSig = sig->getCanonicalSignature();
  return AbstractionPattern(genericSig,
                            decl->getArgumentInterfaceType()
                                ->getCanonicalType());
}

AbstractionPattern::EncodedForeignErrorInfo
AbstractionPattern::EncodedForeignErrorInfo::encode(
                         const Optional<ForeignErrorConvention> &foreignError) {
  EncodedForeignErrorInfo errorInfo;
  if (foreignError.hasValue()) {
    errorInfo =
      EncodedForeignErrorInfo(foreignError->getErrorParameterIndex(),
                              foreignError->isErrorParameterReplacedWithVoid(),
                              foreignError->stripsResultOptionality());
  }
  return errorInfo;
}

AbstractionPattern
AbstractionPattern::getObjCMethod(CanType origType,
                                  const clang::ObjCMethodDecl *method,
                         const Optional<ForeignErrorConvention> &foreignError) {
  auto errorInfo = EncodedForeignErrorInfo::encode(foreignError);
  return getObjCMethod(origType, method, errorInfo);
}

AbstractionPattern
AbstractionPattern::getCurriedObjCMethod(CanType origType,
                                         const clang::ObjCMethodDecl *method,
                         const Optional<ForeignErrorConvention> &foreignError) {
  auto errorInfo = EncodedForeignErrorInfo::encode(foreignError);
  return getCurriedObjCMethod(origType, method, errorInfo);
}

AbstractionPattern
AbstractionPattern::getCurriedCFunctionAsMethod(CanType origType,
                                         const AbstractFunctionDecl *function) {
  auto clangFn = cast<clang::ValueDecl>(function->getClangDecl());
  return getCurriedCFunctionAsMethod(origType,
                     clangFn->getType().getTypePtr(),
                     function->getImportAsMemberStatus());
}

AbstractionPattern
AbstractionPattern::getOptional(AbstractionPattern object) {
  switch (object.getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::Tuple:
  case Kind::PartialCurriedObjCMethodType:
  case Kind::CurriedObjCMethodType:
  case Kind::CFunctionAsMethodType:
  case Kind::PartialCurriedCFunctionAsMethodType:
  case Kind::CurriedCFunctionAsMethodType:
  case Kind::ObjCMethodType:
    llvm_unreachable("cannot add optionality to non-type abstraction");
  case Kind::Opaque:
    return AbstractionPattern::getOpaque();
  case Kind::ClangType:
    return AbstractionPattern(object.getGenericSignature(),
                              OptionalType::get(object.getType())
                                ->getCanonicalType(),
                              object.getClangType());
  case Kind::Type:
    return AbstractionPattern(object.getGenericSignature(),
                              OptionalType::get(object.getType())
                                ->getCanonicalType());
  case Kind::Discard:
    return AbstractionPattern::getDiscard(object.getGenericSignature(),
                              OptionalType::get(object.getType())
                                ->getCanonicalType());
  }
  llvm_unreachable("bad kind");
}

bool AbstractionPattern::isConcreteType() const {
  assert(isTypeParameter());
  return (getKind() != Kind::Opaque &&
          GenericSig != nullptr &&
          GenericSig->isConcreteType(getType()));
}

bool AbstractionPattern::requiresClass() {
  switch (getKind()) {
  case Kind::Opaque:
    return false;
  case Kind::Type:
  case Kind::Discard: {
    auto type = getType();
    if (auto archetype = dyn_cast<ArchetypeType>(type))
      return archetype->requiresClass();
    else if (isa<DependentMemberType>(type) ||
             isa<GenericTypeParamType>(type)) {
      assert(GenericSig &&
             "Dependent type in pattern without generic signature?");
      return GenericSig->requiresClass(type);
    }
    return false;
  }
  default:
    return false;
  }
}

bool AbstractionPattern::matchesTuple(CanTupleType substType) {
  switch (getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::PartialCurriedObjCMethodType:
  case Kind::CurriedObjCMethodType:
  case Kind::PartialCurriedCFunctionAsMethodType:
  case Kind::CurriedCFunctionAsMethodType:
  case Kind::CFunctionAsMethodType:
  case Kind::ObjCMethodType:
    return false;
  case Kind::Opaque:
    return true;
  case Kind::Tuple:
    return getNumTupleElements_Stored() == substType->getNumElements();
  case Kind::ClangType:
  case Kind::Type:
  case Kind::Discard:
    if (isTypeParameter())
      return true;
    auto tuple = dyn_cast<TupleType>(getType());
    return (tuple && tuple->getNumElements() == substType->getNumElements());
  }
  llvm_unreachable("bad kind");
}

static const clang::FunctionType *
getClangFunctionType(const clang::Type *clangType) {
  if (auto ptrTy = clangType->getAs<clang::PointerType>()) {
    clangType = ptrTy->getPointeeType().getTypePtr();
  } else if (auto blockTy = clangType->getAs<clang::BlockPointerType>()) {
    clangType = blockTy->getPointeeType().getTypePtr();
  }
  return clangType->castAs<clang::FunctionType>();
}

static
const clang::Type *getClangFunctionParameterType(const clang::Type *ty,
                                                 unsigned index) {
  // TODO: adjust for error type parameter.

  // If we're asking about parameters, we'd better have a FunctionProtoType.
  auto fnType = getClangFunctionType(ty)->castAs<clang::FunctionProtoType>();
  assert(index < fnType->getNumParams());
  return fnType->getParamType(index).getTypePtr();
}

static
const clang::Type *getClangArrayElementType(const clang::Type *ty,
                                            unsigned index) {
  return ty->castAsArrayTypeUnsafe()->getElementType().getTypePtr();
}

static CanType getCanTupleElementType(CanType type, unsigned index) {
  if (auto tupleTy = dyn_cast<TupleType>(type))
    return tupleTy.getElementType(index);

  assert(index == 0);
  return type;
}

AbstractionPattern
AbstractionPattern::getTupleElementType(unsigned index) const {
  switch (getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::PartialCurriedObjCMethodType:
  case Kind::CurriedObjCMethodType:
  case Kind::PartialCurriedCFunctionAsMethodType:
  case Kind::CurriedCFunctionAsMethodType:
  case Kind::CFunctionAsMethodType:
  case Kind::ObjCMethodType:
    llvm_unreachable("function types are not tuples");
  case Kind::Opaque:
    return *this;
  case Kind::Tuple:
    assert(index < getNumTupleElements_Stored());
    return OrigTupleElements[index];
  case Kind::ClangType:
    return AbstractionPattern(getGenericSignature(),
                              getCanTupleElementType(getType(), index),
                              getClangArrayElementType(getClangType(), index));
  case Kind::Discard:
    llvm_unreachable("operation not needed on discarded abstractions yet");
  case Kind::Type:
    if (isTypeParameter())
      return AbstractionPattern::getOpaque();
    return AbstractionPattern(getGenericSignature(),
                              getCanTupleElementType(getType(), index));
  }
  llvm_unreachable("bad kind");
}

/// Return a pattern corresponding to the 'self' parameter of the given
/// Objective-C method.
AbstractionPattern
AbstractionPattern::getObjCMethodSelfPattern(CanType selfType) const {
  // Just use id for the receiver type.  If this is ever
  // insufficient --- if we have interesting bridging to do to
  // 'self' --- we have the right information to be more exact.
  auto clangSelfType =
    getObjCMethod()->getASTContext().getObjCIdType().getTypePtr();

  return AbstractionPattern(getGenericSignatureForFunctionComponent(),
                            selfType, clangSelfType);
}

/// Return a pattern corresponding to the 'self' parameter of the given
/// C function imported as a method.
AbstractionPattern
AbstractionPattern::getCFunctionAsMethodSelfPattern(CanType selfType) const {
  auto memberStatus = getImportAsMemberStatus();
  if (memberStatus.isInstance()) {
    // Use the clang type for the receiver type.  If this is ever
    // insufficient --- if we have interesting bridging to do to
    // 'self' --- we have the right information to be more exact.
    auto clangSelfType =
      getClangFunctionParameterType(getClangType(),memberStatus.getSelfIndex());

    return AbstractionPattern(getGenericSignatureForFunctionComponent(),
                              selfType, clangSelfType);
  }
  // The formal metatype parameter to a C function imported as a static method
  // is dropped on the floor. Leave it untransformed.
  return AbstractionPattern::getDiscard(
                           getGenericSignatureForFunctionComponent(), selfType);
}

static CanType getResultType(CanType type) {
  return cast<AnyFunctionType>(type).getResult();
}

AbstractionPattern AbstractionPattern::getFunctionResultType() const {
  switch (getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::Tuple:
    llvm_unreachable("abstraction pattern for tuple cannot be function");
  case Kind::Opaque:
    return *this;
  case Kind::Type:
    if (isTypeParameter())
      return AbstractionPattern::getOpaque();
    return AbstractionPattern(getGenericSignatureForFunctionComponent(),
                              getResultType(getType()));
  case Kind::Discard:
    llvm_unreachable("don't need to discard function abstractions yet");
  case Kind::ClangType:
  case Kind::CFunctionAsMethodType:
  case Kind::PartialCurriedCFunctionAsMethodType: {
    auto clangFunctionType = getClangFunctionType(getClangType());
    return AbstractionPattern(getGenericSignatureForFunctionComponent(),
                              getResultType(getType()),
                              clangFunctionType->getReturnType().getTypePtr());    
  }
  case Kind::CurriedObjCMethodType:
    return getPartialCurriedObjCMethod(
                              getGenericSignatureForFunctionComponent(),
                              getResultType(getType()),
                              getObjCMethod(),
                              getEncodedForeignErrorInfo());
  case Kind::CurriedCFunctionAsMethodType:
    return getPartialCurriedCFunctionAsMethod(
                                      getGenericSignatureForFunctionComponent(),
                                      getResultType(getType()),
                                      getClangType(),
                                      getImportAsMemberStatus());
  case Kind::PartialCurriedObjCMethodType:
  case Kind::ObjCMethodType:
    return AbstractionPattern(getGenericSignatureForFunctionComponent(),
                              getResultType(getType()),
                              getObjCMethod()->getReturnType().getTypePtr());
  }
  llvm_unreachable("bad kind");
}

AbstractionPattern
AbstractionPattern::getFunctionParamType(unsigned index) const {
  switch (getKind()) {
  case Kind::Opaque:
    return *this;
  case Kind::Type: {
    if (isTypeParameter())
      return AbstractionPattern::getOpaque();
    auto params = cast<AnyFunctionType>(getType()).getParams();
    return AbstractionPattern(getGenericSignatureForFunctionComponent(),
                              params[index].getParameterType());
  }
  case Kind::CurriedCFunctionAsMethodType: {
    auto params = cast<AnyFunctionType>(getType()).getParams();
    assert(params.size() == 1);
    return getCFunctionAsMethodSelfPattern(params[0].getParameterType());
  }
  case Kind::CFunctionAsMethodType:
  case Kind::PartialCurriedCFunctionAsMethodType: {
    auto params = cast<AnyFunctionType>(getType()).getParams();

    // Only the full method type has a 'self' parameter.
    if (getKind() == Kind::CFunctionAsMethodType) {
      assert(params.size() > 0);

      // The last parameter is 'self'.
      if (index == params.size() - 1) {
        return getCFunctionAsMethodSelfPattern(params.back().getParameterType());
      }
    }

    // A parameter of type () does not correspond to a Clang parameter.
    auto paramType = params[index].getParameterType();
    if (paramType->isVoid())
      return AbstractionPattern(paramType);

    // Otherwise, we're talking about the formal parameter clause.
    // Jump over the self parameter in the Clang type.
    unsigned clangIndex = index;
    auto memberStatus = getImportAsMemberStatus();
    if (memberStatus.isInstance() && clangIndex >= memberStatus.getSelfIndex())
      ++clangIndex;
    return AbstractionPattern(getGenericSignatureForFunctionComponent(),
                              paramType,
                     getClangFunctionParameterType(getClangType(), clangIndex));
  }
  case Kind::CurriedObjCMethodType: {
    auto params = cast<AnyFunctionType>(getType()).getParams();
    assert(params.size() == 1);
    return getObjCMethodSelfPattern(params[0].getParameterType());
  }
  case Kind::ObjCMethodType:
  case Kind::PartialCurriedObjCMethodType: {
    auto params = cast<AnyFunctionType>(getType()).getParams();

    // Only the full method type has a 'self' parameter.
    if (getKind() == Kind::ObjCMethodType) {
      assert(params.size() > 0);

      // The last parameter is 'self'.
      if (index == params.size() - 1) {
        return getObjCMethodSelfPattern(params.back().getParameterType());
      }
    }

    // A parameter of type () does not correspond to a Clang parameter.
    auto paramType = params[index].getParameterType();
    if (paramType->isVoid())
      return AbstractionPattern(paramType);

    // Otherwise, we're talking about the formal parameter clause.
    auto method = getObjCMethod();
    auto errorInfo = getEncodedForeignErrorInfo();

    unsigned paramIndex = index;
    if (errorInfo.hasErrorParameter()) {
      auto errorParamIndex = errorInfo.getErrorParameterIndex();

      if (!errorInfo.isErrorParameterReplacedWithVoid()) {
        if (paramIndex >= errorParamIndex) {
          paramIndex++;
        }
      }
    }

    return AbstractionPattern(getGenericSignatureForFunctionComponent(),
                              paramType,
                      method->parameters()[paramIndex]->getType().getTypePtr());
  }
  case Kind::ClangType: {
    auto params = cast<AnyFunctionType>(getType()).getParams();
    return AbstractionPattern(getGenericSignatureForFunctionComponent(),
                              params[index].getParameterType(),
                          getClangFunctionParameterType(getClangType(), index));
  }
  default:
    llvm_unreachable("does not have function parameters");
  }
}

unsigned AbstractionPattern::getNumFunctionParams() const {
  return cast<AnyFunctionType>(getType()).getParams().size();
}

static CanType getOptionalObjectType(CanType type) {
  auto objectType = type.getOptionalObjectType();
  assert(objectType && "type was not optional");
  return objectType;
}

AbstractionPattern AbstractionPattern::getOptionalObjectType() const {
  switch (getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::ObjCMethodType:
  case Kind::CurriedObjCMethodType:
  case Kind::PartialCurriedObjCMethodType:
  case Kind::CFunctionAsMethodType:
  case Kind::CurriedCFunctionAsMethodType:
  case Kind::PartialCurriedCFunctionAsMethodType:
  case Kind::Tuple:
    llvm_unreachable("pattern for function or tuple cannot be for optional");

  case Kind::Opaque:
    return *this;

  case Kind::Type:
    if (isTypeParameter())
      return AbstractionPattern::getOpaque();
    return AbstractionPattern(getGenericSignature(),
                              ::getOptionalObjectType(getType()));

  case Kind::Discard:
    return AbstractionPattern::getDiscard(getGenericSignature(),
                                          ::getOptionalObjectType(getType()));

  case Kind::ClangType:
    // This is not reflected in clang types.
    return AbstractionPattern(getGenericSignature(),
                              ::getOptionalObjectType(getType()),
                              getClangType());
  }
  llvm_unreachable("bad kind");
}

AbstractionPattern AbstractionPattern::getReferenceStorageReferentType() const {
  switch (getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::Opaque:
  case Kind::ObjCMethodType:
  case Kind::CurriedObjCMethodType:
  case Kind::PartialCurriedObjCMethodType:
  case Kind::CurriedCFunctionAsMethodType:
  case Kind::PartialCurriedCFunctionAsMethodType:
  case Kind::CFunctionAsMethodType:
  case Kind::Tuple:
    return *this;
  case Kind::Type:
    return AbstractionPattern(getGenericSignature(),
                              getType().getReferenceStorageReferent());
  case Kind::Discard:
    return AbstractionPattern::getDiscard(getGenericSignature(),
                                       getType().getReferenceStorageReferent());
  case Kind::ClangType:
    // This is not reflected in clang types.
    return AbstractionPattern(getGenericSignature(),
                              getType().getReferenceStorageReferent(),
                              getClangType());
  }
  llvm_unreachable("bad kind");
}

void AbstractionPattern::dump() const {
  print(llvm::errs());
  llvm::errs() << "\n";
}

void AbstractionPattern::print(raw_ostream &out) const {
  switch (getKind()) {
  case Kind::Invalid:
    out << "AP::Invalid";
    return;
  case Kind::Opaque:
    out << "AP::Opaque";
    return;
  case Kind::Type:
  case Kind::Discard:
    out << (getKind() == Kind::Type
              ? "AP::Type" :
            getKind() == Kind::Discard
              ? "AP::Discard" : "<<UNHANDLED CASE>>");
    if (auto sig = getGenericSignature()) {
      sig->print(out);
    }
    out << '(';
    getType().dump(out);
    out << ')';
    return;
  case Kind::Tuple:
    out << "AP::Tuple(";
    for (unsigned i = 0, e = getNumTupleElements(); i != e; ++i) {
      if (i != 0) out << ", ";
      getTupleElementType(i).print(out);
    }
    out << ")";
    return;
  case Kind::ClangType:
  case Kind::CurriedCFunctionAsMethodType:
  case Kind::PartialCurriedCFunctionAsMethodType:
  case Kind::CFunctionAsMethodType:
    out << (getKind() == Kind::ClangType
              ? "AP::ClangType(" :
            getKind() == Kind::CurriedCFunctionAsMethodType
              ? "AP::CurriedCFunctionAsMethodType(" :
            getKind() == Kind::CFunctionAsMethodType
              ? "AP::CFunctionAsMethodType("
              : "AP::PartialCurriedCFunctionAsMethodType(");
    getType().dump(out);
    out << ", ";
    // It would be better to use print, but we need a PrintingPolicy
    // for that, for which we need a clang LangOptions, and... ugh.
    clang::QualType(getClangType(), 0).dump();
    if (hasImportAsMemberStatus()) {
      out << ", member=";
      auto status = getImportAsMemberStatus();
      if (status.isInstance()) {
        out << "instance, self=" << status.getSelfIndex();
      } else if (status.isStatic()) {
        out << "static";
      }
    }
    out << ")";
    return;
  case Kind::CurriedObjCMethodType:
  case Kind::PartialCurriedObjCMethodType:
  case Kind::ObjCMethodType:
    out << (getKind() == Kind::ObjCMethodType
              ? "AP::ObjCMethodType(" :
            getKind() == Kind::CurriedObjCMethodType
              ? "AP::CurriedObjCMethodType("
              : "AP::PartialCurriedObjCMethodType(");
    getType().dump(out);
    auto errorInfo = getEncodedForeignErrorInfo();
    if (errorInfo.hasValue()) {
      if (errorInfo.hasErrorParameter())
        out << ", errorParameter=" << errorInfo.getErrorParameterIndex();
      if (errorInfo.isErrorParameterReplacedWithVoid())
        out << ", replacedWithVoid";
      if (errorInfo.stripsResultOptionality())
        out << ", stripsResultOptionality";
    }
    out << ", ";
    getObjCMethod()->dump(out);
    out << ")";
    return;
  }
  llvm_unreachable("bad kind");
}

bool AbstractionPattern::hasSameBasicTypeStructure(CanType l, CanType r) {
  if (l == r) return true;

  // Tuples must match.
  auto lTuple = dyn_cast<TupleType>(l);
  auto rTuple = dyn_cast<TupleType>(r);
  if (lTuple && rTuple) {
    auto lElts = lTuple.getElementTypes();
    auto rElts = rTuple.getElementTypes();
    if (lElts.size() != rElts.size())
      return false;
    for (auto i : indices(lElts)) {
      if (!hasSameBasicTypeStructure(lElts[i], rElts[i]))
        return false;
    }
    return true;
  } else if (lTuple || rTuple) {
    return false;
  }

  // Functions must match.
  auto lFunction = dyn_cast<AnyFunctionType>(l);
  auto rFunction = dyn_cast<AnyFunctionType>(r);
  if (lFunction && rFunction) {
    auto lParam = lFunction.getParams();
    auto rParam = rFunction.getParams();
    if (lParam.size() != rParam.size())
      return false;

    for (unsigned i : indices(lParam)) {
      if (!hasSameBasicTypeStructure(lParam[i].getPlainType(),
                                     rParam[i].getPlainType()))
        return false;
    }

    return hasSameBasicTypeStructure(lFunction.getResult(),
                                     rFunction.getResult());
  } else if (lFunction || rFunction) {
    return false;
  }

  // Optionals must match, sortof.
  auto lObject = l.getOptionalObjectType();
  auto rObject = r.getOptionalObjectType();
  if (lObject && rObject) {
    return hasSameBasicTypeStructure(lObject, rObject);
  } else if (lObject || rObject) {
    // Allow optionality mis-matches, but require the underlying types to match.
    return hasSameBasicTypeStructure(lObject ? lObject : l,
                                     rObject ? rObject : r);
  }

  // Otherwise, the structure is similar enough.
  return true;
}
