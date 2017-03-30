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
#include "swift/AST/ForeignErrorConvention.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/PrettyPrinter.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

using namespace swift;
using namespace swift::Lowering;

AbstractionPattern TypeConverter::getAbstractionPattern(AbstractStorageDecl *decl) {
  if (auto var = dyn_cast<VarDecl>(decl)) {
    return getAbstractionPattern(var);
  } else {
    return getAbstractionPattern(cast<SubscriptDecl>(decl));
  }
}

AbstractionPattern TypeConverter::getAbstractionPattern(SubscriptDecl *decl) {
  CanGenericSignature genericSig;
  if (auto sig = decl->getGenericSignatureOfContext())
    genericSig = sig->getCanonicalSignature();
  return AbstractionPattern(genericSig,
                            decl->getElementInterfaceType()
                                ->getCanonicalType());
}

AbstractionPattern
TypeConverter::getIndicesAbstractionPattern(SubscriptDecl *decl) {
  CanGenericSignature genericSig;
  if (auto sig = decl->getGenericSignatureOfContext())
    genericSig = sig->getCanonicalSignature();
  auto indicesTy = decl->getIndicesInterfaceType();
  auto indicesCanTy = indicesTy->getCanonicalType(genericSig,
                                                  *decl->getParentModule());
  return AbstractionPattern(genericSig, indicesCanTy);
}

static const clang::Type *getClangType(const clang::Decl *decl) {
  if (auto valueDecl = dyn_cast<clang::ValueDecl>(decl)) {
    return valueDecl->getType().getTypePtr();
  }

  // This should *really* be a ValueDecl.
  return cast<clang::ObjCPropertyDecl>(decl)->getType().getTypePtr();
}

AbstractionPattern TypeConverter::getAbstractionPattern(VarDecl *var) {
  CanGenericSignature genericSig;
  if (auto sig = var->getDeclContext()->getGenericSignatureOfContext())
    genericSig = sig->getCanonicalSignature();

  CanType swiftType = var->getInterfaceType()->getCanonicalType();
  if (auto inout = dyn_cast<InOutType>(swiftType))
    swiftType = inout.getObjectType();

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
  assert(decl->getArgumentInterfaceType());
  assert(!decl->hasClangNode());

  // This cannot be implemented correctly for Optional.Some.
  assert(decl->getParentEnum()->classifyAsOptionalType() == OTK_None &&
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
AbstractionPattern::getOptional(AbstractionPattern object,
                                OptionalTypeKind optionalKind) {
  switch (object.getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::Tuple:
  case Kind::PartialCurriedObjCMethodType:
  case Kind::CurriedObjCMethodType:
  case Kind::PartialCurriedCFunctionAsMethodType:
  case Kind::CurriedCFunctionAsMethodType:
  case Kind::ObjCMethodType:
  case Kind::ObjCMethodParamTupleType:
  case Kind::ObjCMethodFormalParamTupleType:
  case Kind::CFunctionAsMethodFormalParamTupleType:
  case Kind::ClangFunctionParamTupleType:
    llvm_unreachable("cannot add optionality to non-type abstraction");
  case Kind::Opaque:
    return AbstractionPattern::getOpaque();
  case Kind::ClangType:
    return AbstractionPattern(object.getGenericSignature(),
                              OptionalType::get(optionalKind, object.getType())
                                ->getCanonicalType(),
                              object.getClangType());
  case Kind::Type:
    return AbstractionPattern(object.getGenericSignature(),
                              OptionalType::get(optionalKind, object.getType())
                                ->getCanonicalType());
  case Kind::Discard:
    return AbstractionPattern::getDiscard(object.getGenericSignature(),
                              OptionalType::get(optionalKind, object.getType())
                                ->getCanonicalType());
  }
  llvm_unreachable("bad kind");
}

bool AbstractionPattern::matchesTuple(CanTupleType substType) {
  switch (getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::PartialCurriedObjCMethodType:
  case Kind::CurriedObjCMethodType:
  case Kind::PartialCurriedCFunctionAsMethodType:
  case Kind::CurriedCFunctionAsMethodType:
  case Kind::ObjCMethodType:
    return false;
  case Kind::Opaque:
    return true;
  case Kind::Tuple:
    return getNumTupleElements_Stored() == substType->getNumElements();
  case Kind::ObjCMethodParamTupleType:
  case Kind::ObjCMethodFormalParamTupleType:
  case Kind::CFunctionAsMethodFormalParamTupleType:
  case Kind::ClangFunctionParamTupleType:
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

static bool isVoidLike(CanType type) {
  return (type->isVoid() ||
          (isa<TupleType>(type) &&
           cast<TupleType>(type)->getNumElements() == 1 &&
           cast<TupleType>(type).getElementType(0)->isVoid()));
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
  case Kind::ClangFunctionParamTupleType: {
    // Handle the (label: ()) param used by functions imported as labeled
    // nullary initializers.
    if (isVoidLike(getType()))
      return AbstractionPattern(getType()->getASTContext().TheEmptyTupleType);
    
    return AbstractionPattern(getGenericSignature(),
                              getCanTupleElementType(getType(), index),
                          getClangFunctionParameterType(getClangType(), index));
  }

  case Kind::ObjCMethodFormalParamTupleType: {
    auto swiftEltType = getCanTupleElementType(getType(), index);
    auto method = getObjCMethod();
    auto errorInfo = getEncodedForeignErrorInfo();

    // If we're asking for something after the error parameter, slide
    // the parameter index up by one.
    auto paramIndex = index;
    if (errorInfo.hasErrorParameter()) {
      auto errorParamIndex = errorInfo.getErrorParameterIndex();
      if (errorInfo.isErrorParameterReplacedWithVoid()) {
        if (paramIndex == errorParamIndex) {
          assert(isVoidLike(swiftEltType));
          (void)&isVoidLike;
          return AbstractionPattern(swiftEltType);
        }
      } else {
        if (paramIndex >= errorParamIndex) {
          paramIndex++;
        }
      }
    }

    return AbstractionPattern(getGenericSignature(), swiftEltType,
                  method->parameters()[paramIndex]->getType().getTypePtr());
  }
  case Kind::CFunctionAsMethodFormalParamTupleType: {
    // Jump over the self parameter in the Clang type.
    unsigned clangIndex = index;
    auto memberStatus = getImportAsMemberStatus();
    if (memberStatus.isInstance() && clangIndex >= memberStatus.getSelfIndex())
      ++clangIndex;
    return AbstractionPattern(getGenericSignature(),
                              getCanTupleElementType(getType(), index),
                     getClangFunctionParameterType(getClangType(), clangIndex));
  }
  case Kind::ObjCMethodParamTupleType: {
    auto tupleType = cast<TupleType>(getType());
    assert(tupleType->getNumElements() == 2);
    assert(index < 2);

    auto swiftEltType = tupleType.getElementType(index);
    if (index != 0) {
      return getObjCMethodSelfPattern(swiftEltType);
    }

    // Otherwise, we're talking about the formal parameter clause.
    return getObjCMethodFormalParamPattern(swiftEltType);
  }
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

/// Return a pattern corresponding to the formal method parameters of
/// the current C function imported as a method.
AbstractionPattern AbstractionPattern::
getCFunctionAsMethodFormalParamPattern(CanType paramType) const {
  auto sig = getGenericSignatureForFunctionComponent();
  auto clangType = getClangType();
  
  // Nullary methods still take a formal () parameter clause.
  // There's no corresponding Clang type for that.
  if (isVoidLike(paramType))
    return AbstractionPattern(paramType);
  
  // If we imported as a tuple type, construct the special
  // method-formal-parameters abstraction pattern.
  if (isa<TupleType>(paramType)) {
    return getCFunctionAsMethodFormalParamTuple(sig, paramType,
                                                clangType,
                                                getImportAsMemberStatus());
  }
  
  // Otherwise, we imported a single parameter.
  // Get the non-self parameter from the Clang type.
  unsigned paramIndex = 0;
  auto selfIndex = getImportAsMemberStatus();
  if (selfIndex.isInstance() && selfIndex.getSelfIndex() == 0)
    paramIndex = 1;
  
  return AbstractionPattern(sig, paramType,
                          getClangFunctionParameterType(clangType, paramIndex));
}

/// Return a pattern corresponding to the formal parameters of the
/// current Objective-C method.
AbstractionPattern
AbstractionPattern::getObjCMethodFormalParamPattern(CanType inputType) const {
  auto signature = getGenericSignatureForFunctionComponent();
  auto method = getObjCMethod();
  auto errorInfo = getEncodedForeignErrorInfo();

  // Nullary methods still take a formal () parameter clause.
  // There's no corresponding Clang type for that.
  if (method->parameters().empty() ||
      (method->parameters().size() == 1 &&
       errorInfo.hasErrorParameter())) {
    // Imported initializers also sometimes get "withFooBar: ()" clauses.
    assert(isVoidLike(inputType));
    return AbstractionPattern(inputType);
  }

  // If we imported as a tuple type, construct the special
  // method-formal-parameters abstraction pattern.
  if (isa<TupleType>(inputType)) {
    // This assertion gets messed up by variadic methods that we've
    // imported as non-variadic.
    assert(method->isVariadic() ||
           method->parameters().size() ==
             cast<TupleType>(inputType)->getNumElements()
             + unsigned(errorInfo.hasUnreplacedErrorParameter()));
    return getObjCMethodFormalParamTuple(signature, inputType,
                                         method, errorInfo);
  }

  // Otherwise, we must have imported a single parameter.
  // But we might also have a foreign error.

  // If we don't, we must have a single source parameter.
  if (!errorInfo.hasErrorParameter()) {
    assert(method->parameters().size() == 1);
    return AbstractionPattern(signature, inputType,
                            method->parameters()[0]->getType().getTypePtr());
  }

  // Otherwise, we must have two; pick the one that isn't the foreign error.
  assert(method->parameters().size() == 2);
  unsigned errorIndex = errorInfo.getErrorParameterIndex();
  assert(errorIndex < 2);
  unsigned paramIndex = (errorIndex == 0 ? 1 : 0);
  return AbstractionPattern(signature, inputType,
                   method->parameters()[paramIndex]->getType().getTypePtr());
}

AbstractionPattern AbstractionPattern::transformType(
                       llvm::function_ref<CanType(CanType)> transform) const {
  switch (getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::Tuple:
    return *this;
  case Kind::Opaque:
    return getOpaque();
  case Kind::PartialCurriedObjCMethodType:
    return getPartialCurriedObjCMethod(getGenericSignature(),
                                       transform(getType()), getObjCMethod(),
                                       getEncodedForeignErrorInfo());
  case Kind::CurriedObjCMethodType:
    return getCurriedObjCMethod(transform(getType()), getObjCMethod(),
                                getEncodedForeignErrorInfo());
  case Kind::PartialCurriedCFunctionAsMethodType:
    return getPartialCurriedCFunctionAsMethod(getGenericSignature(),
                                              transform(getType()),
                                              getClangType(),
                                              getImportAsMemberStatus());
  case Kind::CurriedCFunctionAsMethodType:
    return getCurriedCFunctionAsMethod(transform(getType()), getClangType(),
                                       getImportAsMemberStatus());
  case Kind::ObjCMethodType:
    return getObjCMethod(transform(getType()), getObjCMethod(),
                         getEncodedForeignErrorInfo());
  case Kind::ClangType:
    return AbstractionPattern(getGenericSignature(),
                              transform(getType()), getClangType());
  case Kind::Type:
    return AbstractionPattern(getGenericSignature(), transform(getType()));
  case Kind::Discard:
    return AbstractionPattern::getDiscard(getGenericSignature(),
                                          transform(getType()));
  case Kind::ObjCMethodParamTupleType:
    return getObjCMethodParamTuple(getGenericSignature(),
                                   transform(getType()), getObjCMethod(),
                                   getEncodedForeignErrorInfo());

  // In both of the following cases, if the transform makes it no
  // longer a tuple type, we need to change kinds.
  case Kind::ClangFunctionParamTupleType: {
    auto newType = transform(getType());
    if (isa<TupleType>(newType)) {
      return getClangFunctionParamTuple(getGenericSignature(),
                                        newType, getClangType());
    } else {
      assert(getNumTupleElements() == 1);
      return AbstractionPattern(getGenericSignature(), newType,
                             getClangFunctionParameterType(getClangType(), 0));
    }
  }
  case Kind::ObjCMethodFormalParamTupleType: {
    auto newType = transform(getType());
    if (isa<TupleType>(newType)) {
      return getObjCMethodFormalParamTuple(getGenericSignature(),
                                           newType, getObjCMethod(),
                                           getEncodedForeignErrorInfo());
    } else {
      assert(getNumTupleElements() == 1);
      return AbstractionPattern(getGenericSignature(), newType,
                   getObjCMethod()->parameters()[0]->getType().getTypePtr());
    }
  }
  case Kind::CFunctionAsMethodFormalParamTupleType: {
    auto newType = transform(getType());
    if (isa<TupleType>(newType)) {
      return getCFunctionAsMethodFormalParamTuple(getGenericSignature(),
                                                  newType, getClangType(),
                                                  getImportAsMemberStatus());
    }
  }
  }
  llvm_unreachable("bad kind");
}

static CanType dropLastElement(CanType type) {
  auto elts = cast<TupleType>(type)->getElements().drop_back();
  return TupleType::get(elts, type->getASTContext())->getCanonicalType();
}

AbstractionPattern AbstractionPattern::dropLastTupleElement() const {
  switch (getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::Tuple: {
    auto n = getNumTupleElements_Stored();
    return getTuple(llvm::makeArrayRef(OrigTupleElements, n - 1));
  }
  case Kind::Opaque:
    return getOpaque();
  case Kind::CurriedObjCMethodType:
  case Kind::PartialCurriedObjCMethodType:
  case Kind::CurriedCFunctionAsMethodType:
  case Kind::PartialCurriedCFunctionAsMethodType:
  case Kind::ObjCMethodType:
    llvm_unreachable("not a tuple type");
  case Kind::ClangType:
    llvm_unreachable("dropping last element of imported array?");
  case Kind::ObjCMethodParamTupleType:
  case Kind::ObjCMethodFormalParamTupleType:
  case Kind::CFunctionAsMethodFormalParamTupleType:
    llvm_unreachable("operation is not needed on method abstraction patterns");
  case Kind::Type:
    if (isTypeParameter())
      return AbstractionPattern::getOpaque();
    return AbstractionPattern(getGenericSignature(),
                              dropLastElement(getType()));
  case Kind::Discard:
    llvm_unreachable("don't need to drop element on discarded abstractions "
                     "yet");
  // In both of the following cases, if the transform makes it no
  // longer a tuple type, we need to change kinds.
  case Kind::ClangFunctionParamTupleType: {
    auto newType = dropLastElement(getType());
    if (isa<TupleType>(newType)) {
      return getClangFunctionParamTuple(getGenericSignature(),
                                        newType, getClangType());
    } else {
      assert(getNumTupleElements() == 2);
      return AbstractionPattern(getGenericSignature(), newType,
                             getClangFunctionParameterType(getClangType(), 0));
    }
  }
  }
  llvm_unreachable("bad kind");  
}

AbstractionPattern AbstractionPattern::getLValueOrInOutObjectType() const {
  switch (getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::Tuple:
  case Kind::ClangFunctionParamTupleType:
  case Kind::PartialCurriedObjCMethodType:
  case Kind::CurriedObjCMethodType:
  case Kind::CurriedCFunctionAsMethodType:
  case Kind::PartialCurriedCFunctionAsMethodType:
  case Kind::ObjCMethodType:
  case Kind::ObjCMethodParamTupleType:
  case Kind::ObjCMethodFormalParamTupleType:
  case Kind::CFunctionAsMethodFormalParamTupleType:
    llvm_unreachable("abstraction pattern for lvalue cannot be tuple");
  case Kind::Opaque:
    return *this;
  case Kind::Type:
    return AbstractionPattern(getGenericSignature(),
                              getType().getLValueOrInOutObjectType());
  case Kind::Discard:
    return AbstractionPattern::getDiscard(getGenericSignature(),
                                        getType().getLValueOrInOutObjectType());
  case Kind::ClangType:
    return AbstractionPattern(getGenericSignature(),
                              getType().getLValueOrInOutObjectType(),
                              getClangType());
  }
  llvm_unreachable("bad kind");
}

static CanType getResultType(CanType type) {
  return cast<AnyFunctionType>(type).getResult();
}

AbstractionPattern AbstractionPattern::getFunctionResultType() const {
  switch (getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::ClangFunctionParamTupleType:
  case Kind::ObjCMethodParamTupleType:
  case Kind::ObjCMethodFormalParamTupleType:
  case Kind::CFunctionAsMethodFormalParamTupleType:
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

AbstractionPattern AbstractionPattern::getFunctionInputType() const {
  switch (getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::ClangFunctionParamTupleType:
  case Kind::ObjCMethodParamTupleType:
  case Kind::ObjCMethodFormalParamTupleType:
  case Kind::CFunctionAsMethodFormalParamTupleType:
  case Kind::Tuple:
    llvm_unreachable("abstraction pattern for tuple cannot be function");
  case Kind::Opaque:
    return *this;
  case Kind::Type:
    if (isTypeParameter())
      return AbstractionPattern::getOpaque();
    return AbstractionPattern(getGenericSignatureForFunctionComponent(),
                              cast<AnyFunctionType>(getType()).getInput());
  case Kind::Discard:
    llvm_unreachable("don't need to discard function abstractions yet");
  case Kind::ClangType: {
    // Preserve the Clang type in the resulting abstraction pattern.
    auto inputType = cast<AnyFunctionType>(getType()).getInput();
    if (isa<TupleType>(inputType)) {
      return getClangFunctionParamTuple(
                                 getGenericSignatureForFunctionComponent(),
                                        inputType, getClangType());
    } else {
      return AbstractionPattern(getGenericSignatureForFunctionComponent(),
                                inputType,
                                getClangFunctionParameterType(getClangType(), 0));
    }
  }
  case Kind::CurriedCFunctionAsMethodType:
    return getCFunctionAsMethodSelfPattern(
                                cast<AnyFunctionType>(getType()).getInput());
  case Kind::PartialCurriedCFunctionAsMethodType:
    return getCFunctionAsMethodFormalParamPattern(
                                cast<AnyFunctionType>(getType()).getInput());
  case Kind::CurriedObjCMethodType:
    return getObjCMethodSelfPattern(
                                cast<AnyFunctionType>(getType()).getInput());
  case Kind::PartialCurriedObjCMethodType:
    return getObjCMethodFormalParamPattern(
                                cast<AnyFunctionType>(getType()).getInput());
  case Kind::ObjCMethodType: {
    // Preserve the Clang type in the resulting abstraction pattern.
    auto inputType = cast<AnyFunctionType>(getType()).getInput();
    assert(isa<TupleType>(inputType)); // always at least ((), SelfType)
    return getObjCMethodParamTuple(getGenericSignatureForFunctionComponent(),
                                   inputType, getObjCMethod(),
                                   getEncodedForeignErrorInfo());
  }
  }
  llvm_unreachable("bad kind");
}

static CanType getAnyOptionalObjectType(CanType type) {
  auto objectType = type.getAnyOptionalObjectType();
  assert(objectType && "type was not optional");
  return objectType;
}

AbstractionPattern AbstractionPattern::getAnyOptionalObjectType() const {
  switch (getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::ClangFunctionParamTupleType:
  case Kind::ObjCMethodParamTupleType:
  case Kind::ObjCMethodFormalParamTupleType:
  case Kind::ObjCMethodType:
  case Kind::CurriedObjCMethodType:
  case Kind::PartialCurriedObjCMethodType:
  case Kind::CurriedCFunctionAsMethodType:
  case Kind::PartialCurriedCFunctionAsMethodType:
  case Kind::Tuple:
  case Kind::CFunctionAsMethodFormalParamTupleType:
    llvm_unreachable("pattern for function or tuple cannot be for optional");

  case Kind::Opaque:
    return *this;

  case Kind::Type:
    if (isTypeParameter())
      return AbstractionPattern::getOpaque();
    return AbstractionPattern(getGenericSignature(),
                              ::getAnyOptionalObjectType(getType()));

  case Kind::Discard:
    return AbstractionPattern::getDiscard(getGenericSignature(),
                                        ::getAnyOptionalObjectType(getType()));

  case Kind::ClangType:
    // This is not reflected in clang types.
    return AbstractionPattern(getGenericSignature(),
                              ::getAnyOptionalObjectType(getType()),
                              getClangType());
  }
  llvm_unreachable("bad kind");
}

AbstractionPattern AbstractionPattern::getReferenceStorageReferentType() const {
  switch (getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::Opaque:
  case Kind::ClangFunctionParamTupleType:
  case Kind::ObjCMethodParamTupleType:
  case Kind::ObjCMethodFormalParamTupleType:
  case Kind::ObjCMethodType:
  case Kind::CurriedObjCMethodType:
  case Kind::PartialCurriedObjCMethodType:
  case Kind::CurriedCFunctionAsMethodType:
  case Kind::PartialCurriedCFunctionAsMethodType:
  case Kind::Tuple:
  case Kind::CFunctionAsMethodFormalParamTupleType:
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
  case Kind::ClangFunctionParamTupleType:
  case Kind::CurriedCFunctionAsMethodType:
  case Kind::PartialCurriedCFunctionAsMethodType:
  case Kind::CFunctionAsMethodFormalParamTupleType:
    out << (getKind() == Kind::ClangType
              ? "AP::ClangType(" :
            getKind() == Kind::ClangFunctionParamTupleType
              ? "AP::ClangFunctionParamTupleType(" :
            getKind() == Kind::CurriedCFunctionAsMethodType
              ? "AP::CurriedCFunctionAsMethodType(" :
            getKind() == Kind::PartialCurriedCFunctionAsMethodType
              ? "AP::PartialCurriedCFunctionAsMethodType(" :
            getKind() == Kind::CFunctionAsMethodFormalParamTupleType
              ? "AP::CFunctionAsMethodFormalParamTupleType("
              : "<<UNHANDLED CASE>>(");
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
  case Kind::ObjCMethodFormalParamTupleType:
  case Kind::ObjCMethodParamTupleType:
  case Kind::ObjCMethodType:
    out << (getKind() == Kind::ObjCMethodType
              ? "AP::ObjCMethodType(" :
            getKind() == Kind::CurriedObjCMethodType
              ? "AP::CurriedObjCMethodType(" :
            getKind() == Kind::PartialCurriedObjCMethodType
              ? "AP::PartialCurriedObjCMethodType(" :
            getKind() == Kind::ObjCMethodParamTupleType
              ? "AP::ObjCMethodParamTupleType("
              : "AP::ObjCMethodFormalParamTupleType(");
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

