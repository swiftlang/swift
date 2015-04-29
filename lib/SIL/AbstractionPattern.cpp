//===--- AbstractionPattern.cpp - Abstraction patterns --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines routines relating to abstraction patterns.
// working in concert with the Clang importer.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "libsil"
#include "swift/SIL/TypeLowering.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/Basic/Fallthrough.h"
#include "clang/AST/ASTContext.h"
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
  // TODO: honor the declared type?
  return AbstractionPattern(decl->getElementType());
}

static const clang::Type *getClangType(const clang::Decl *decl) {
  if (auto valueDecl = dyn_cast<clang::ValueDecl>(decl)) {
    return valueDecl->getType().getTypePtr();
  }

  // This should *really* be a ValueDecl.
  return cast<clang::ObjCPropertyDecl>(decl)->getType().getTypePtr();
}

AbstractionPattern TypeConverter::getAbstractionPattern(VarDecl *var) {
  CanType swiftType = var->getType()->getCanonicalType();
  if (auto inout = dyn_cast<InOutType>(swiftType)) {
    swiftType = inout.getObjectType();
  }

  if (auto clangDecl = var->getClangDecl()) {
    auto clangType = getClangType(clangDecl);
    swiftType = getLoweredBridgedType(swiftType,
                              SILFunctionTypeRepresentation::CFunctionPointer,
                              clangType,
                              TypeConverter::ForMemory)
      ->getCanonicalType();
    return AbstractionPattern(swiftType, clangType);
  } else {
    return AbstractionPattern(swiftType);
  }
}

AbstractionPattern TypeConverter::getAbstractionPattern(EnumElementDecl *decl) {
  assert(decl->hasArgumentType());
  assert(!decl->hasClangNode());
  return AbstractionPattern(decl->getArgumentType());
}

AbstractionPattern
AbstractionPattern::getObjCMethod(CanType origType,
                                  const clang::ObjCMethodDecl *method,
                         const Optional<ForeignErrorConvention> &foreignError) {
  if (foreignError.hasValue()) {
    return getObjCMethod(origType, method,
                         { foreignError->getErrorParameterIndex() + 1 });
  } else {
    return getObjCMethod(origType, method, { 0 });
  }
}

AbstractionPattern
AbstractionPattern::getOptional(AbstractionPattern object,
                                OptionalTypeKind optionalKind) {
  switch (object.getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::Tuple:
  case Kind::ObjCMethodType:
  case Kind::ObjCMethodParamTupleType:
  case Kind::ObjCMethodFormalParamTupleType:
  case Kind::ClangFunctionParamTupleType:
    llvm_unreachable("cannot add optionality to non-type abstraction");
  case Kind::Opaque:
    return AbstractionPattern::getOpaque();
  case Kind::ClangType:
    return AbstractionPattern(OptionalType::get(optionalKind, object.getType())
                                ->getCanonicalType(),
                              object.getClangType());
  case Kind::Type:
    return AbstractionPattern(object.getGenericSignature(),
                              OptionalType::get(optionalKind, object.getType())
                                ->getCanonicalType());
  }
  llvm_unreachable("bad kind");
}

bool AbstractionPattern::matchesTuple(CanTupleType substType) {
  switch (getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::ObjCMethodType:
    return false;
  case Kind::Opaque:
    return true;
  case Kind::Tuple:
    return getNumTupleElements_Stored() == substType->getNumElements();
  case Kind::ObjCMethodParamTupleType:
  case Kind::ObjCMethodFormalParamTupleType:
  case Kind::ClangFunctionParamTupleType:
  case Kind::ClangType:
  case Kind::Type:
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
  auto fnType = cast<clang::FunctionProtoType>(getClangFunctionType(ty));
  assert(index < fnType->getNumParams());
  return fnType->getParamType(index).getTypePtr();
}

static
const clang::Type *getClangArrayElementType(const clang::Type *ty,
                                            unsigned index) {
  return cast<clang::ArrayType>(ty)->getElementType().getTypePtr();
}

AbstractionPattern
AbstractionPattern::getTupleElementType(unsigned index) const {
  switch (getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::ObjCMethodType:
    llvm_unreachable("arbitrary clang types are not imported as tuples");
  case Kind::Opaque:
    return *this;
  case Kind::Tuple:
    assert(index < getNumTupleElements_Stored());
    return OrigTupleElements[index];
  case Kind::ClangType:
    return AbstractionPattern(cast<TupleType>(getType()).getElementType(index),
                              getClangArrayElementType(getClangType(), index));
  case Kind::Type:
    return AbstractionPattern(getGenericSignature(),
                              cast<TupleType>(getType()).getElementType(index));
  case Kind::ClangFunctionParamTupleType:
    return AbstractionPattern(cast<TupleType>(getType()).getElementType(index),
                          getClangFunctionParameterType(getClangType(), index));

  case Kind::ObjCMethodFormalParamTupleType: {
    auto swiftEltType = cast<TupleType>(getType()).getElementType(index);
    auto method = getObjCMethod();
    auto errorInfo = getEncodedForeignErrorInfo();

    // If we're asking for something after the error parameter, slide
    // the parameter index up by one.
    auto paramIndex = index;
    if (errorInfo.hasErrorParameter() &&
        paramIndex >= errorInfo.getErrorParameterIndex()) {
      paramIndex++;
    }

    return AbstractionPattern(swiftEltType,
                  method->parameters()[paramIndex]->getType().getTypePtr());
  }
  case Kind::ObjCMethodParamTupleType: {
    auto tupleType = cast<TupleType>(getType());
    assert(tupleType->getNumElements() == 2);
    assert(index < 2);

    auto method = getObjCMethod();
    auto swiftEltType = tupleType.getElementType(index);
    if (index != 0) {
      // Just use id for the receiver type.  If this is ever
      // insufficient --- if we have interesting bridging to do to
      // 'self' --- we have the right information to be more exact.
      return AbstractionPattern(swiftEltType,
                       method->getASTContext().getObjCIdType().getTypePtr());
    }

    // Otherwise, we're talking about the formal parameter clause.

    auto errorInfo = getEncodedForeignErrorInfo();

    // Nullary methods still take a formal () parameter clause.
    // There's no corresponding Clang type for that.
    if (method->parameters().empty() ||
        (method->parameters().size() == 1 &&
         errorInfo.hasErrorParameter())) {
      // Imported initializers also sometimes get "withFooBar: ()" clauses.
      assert(swiftEltType->isVoid() ||
             (isa<TupleType>(swiftEltType) &&
              cast<TupleType>(swiftEltType)->getNumElements() == 1 &&
              cast<TupleType>(swiftEltType).getElementType(0)->isVoid()));
      return AbstractionPattern(swiftEltType);
    }

    // If we imported as a tuple type, construct the special
    // method-formal-parameters abstraction pattern.
    if (isa<TupleType>(swiftEltType)) {
      // This assertion gets messed up by variadic methods that we've
      // imported as non-variadic.
      assert(method->isVariadic() ||
             method->parameters().size() ==
             cast<TupleType>(swiftEltType)->getNumElements() +
               unsigned(errorInfo.hasErrorParameter()));
      return getObjCMethodFormalParamTuple(swiftEltType, method, errorInfo);
    }

    // Otherwise, we must have imported a single parameter.
    // But we might also have a foreign error.

    // If we don't, we must have a single source parameter.
    if (!errorInfo.hasErrorParameter()) {
      assert(method->parameters().size() == 1);
      return AbstractionPattern(swiftEltType,
                            method->parameters()[0]->getType().getTypePtr());
    }

    // Otherwise, we must have two; pick the one that isn't the foreign error.
    assert(method->parameters().size() == 2);
    unsigned errorIndex = errorInfo.getErrorParameterIndex();
    assert(errorIndex < 2);
    unsigned paramIndex = (errorIndex == 0 ? 1 : 0);
    return AbstractionPattern(swiftEltType,
                   method->parameters()[paramIndex]->getType().getTypePtr());
  }
  }
  llvm_unreachable("bad kind");
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
  case Kind::ObjCMethodType:
    return getObjCMethod(transform(getType()), getObjCMethod(),
                         getEncodedForeignErrorInfo());
  case Kind::ClangType:
    return AbstractionPattern(transform(getType()), getClangType());
  case Kind::Type:
    return AbstractionPattern(getGenericSignature(), transform(getType()));
  case Kind::ObjCMethodParamTupleType:
    return getObjCMethodParamTuple(transform(getType()), getObjCMethod(),
                                   getEncodedForeignErrorInfo());

  // In both of the following cases, if the transform makes it no
  // longer a tuple type, we need to change kinds.
  case Kind::ClangFunctionParamTupleType: {
    auto newType = transform(getType());
    if (isa<TupleType>(newType)) {
      return getClangFunctionParamTuple(newType, getClangType());
    } else {
      assert(getNumTupleElements() == 1);
      return AbstractionPattern(newType,
                             getClangFunctionParameterType(getClangType(), 0));
    }
  }
  case Kind::ObjCMethodFormalParamTupleType: {
    auto newType = transform(getType());
    if (isa<TupleType>(newType)) {
      return getObjCMethodFormalParamTuple(newType, getObjCMethod(),
                                           getEncodedForeignErrorInfo());
    } else {
      assert(getNumTupleElements() == 1);
      return AbstractionPattern(newType,
                   getObjCMethod()->parameters()[0]->getType().getTypePtr());
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
  case Kind::ObjCMethodType:
    llvm_unreachable("not a tuple type");
  case Kind::ClangType:
    llvm_unreachable("dropping last element of imported array?");
  case Kind::ObjCMethodParamTupleType:
  case Kind::ObjCMethodFormalParamTupleType:
    llvm_unreachable("operation is not needed on method abstraction patterns");
  case Kind::Type:
    return AbstractionPattern(getGenericSignature(),
                              dropLastElement(getType()));

  // In both of the following cases, if the transform makes it no
  // longer a tuple type, we need to change kinds.
  case Kind::ClangFunctionParamTupleType: {
    auto newType = dropLastElement(getType());
    if (isa<TupleType>(newType)) {
      return getClangFunctionParamTuple(newType, getClangType());
    } else {
      assert(getNumTupleElements() == 2);
      return AbstractionPattern(newType,
                             getClangFunctionParameterType(getClangType(), 0));
    }
  }
  }
  llvm_unreachable("bad kind");  
}

AbstractionPattern AbstractionPattern::getLValueObjectType() const {
  switch (getKind()) {
  case Kind::Invalid:
    llvm_unreachable("querying invalid abstraction pattern!");
  case Kind::Tuple:
  case Kind::ClangFunctionParamTupleType:
  case Kind::ObjCMethodType:
  case Kind::ObjCMethodParamTupleType:
  case Kind::ObjCMethodFormalParamTupleType:
    llvm_unreachable("abstraction pattern for lvalue cannot be tuple");
  case Kind::Opaque:
    return *this;
  case Kind::Type:
    return AbstractionPattern(getGenericSignature(),
                              cast<InOutType>(getType()).getObjectType());
  case Kind::ClangType:
    return AbstractionPattern(cast<InOutType>(getType()).getObjectType(),
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
  case Kind::Tuple:
    llvm_unreachable("abstraction pattern for tuple cannot be function");
  case Kind::Opaque:
    return *this;
  case Kind::Type: {
    auto fnType = cast<AnyFunctionType>(getType());
    if (auto genericFn = dyn_cast<GenericFunctionType>(fnType)) {
      return AbstractionPattern(genericFn.getGenericSignature(),
                                fnType.getResult());
    } else {
      return AbstractionPattern(getGenericSignature(), fnType.getResult());
    }
  }
  case Kind::ClangType: {
    auto clangFunctionType = getClangFunctionType(getClangType());
    return AbstractionPattern(getResultType(getType()),
                              clangFunctionType->getReturnType().getTypePtr());    
  }
  case Kind::ObjCMethodType:
    return AbstractionPattern(getResultType(getType()),
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
  case Kind::Tuple:
    llvm_unreachable("abstraction pattern for tuple cannot be function");
  case Kind::Opaque:
    return *this;
  case Kind::Type: {
    auto fnType = cast<AnyFunctionType>(getType());
    if (auto genericFn = dyn_cast<GenericFunctionType>(fnType)) {
      return AbstractionPattern(genericFn.getGenericSignature(),
                                fnType.getInput());
    } else {
      return AbstractionPattern(getGenericSignature(), fnType.getInput());
    }
  }
  case Kind::ClangType: {
    // Preserve the Clang type in the resulting abstraction pattern.
    auto inputType = cast<AnyFunctionType>(getType()).getInput();
    if (isa<TupleType>(inputType)) {
      return AbstractionPattern::getClangFunctionParamTuple(inputType, getClangType());
    } else {
      return AbstractionPattern(inputType,
                                getClangFunctionParameterType(getClangType(), 0));
    }
  }
  case Kind::ObjCMethodType: {
    // Preserve the Clang type in the resulting abstraction pattern.
    auto inputType = cast<AnyFunctionType>(getType()).getInput();
    assert(isa<TupleType>(inputType)); // always at least ((), SelfType)
    return getObjCMethodParamTuple(inputType, getObjCMethod(),
                                   getEncodedForeignErrorInfo());
  }
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
  case Kind::Tuple:
    return *this;
  case Kind::Type:
    return AbstractionPattern(getGenericSignature(),
                              getType().getReferenceStorageReferent());
  case Kind::ClangType:
    // This is not reflected in clang types.
    return AbstractionPattern(getType().getReferenceStorageReferent(),
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
    out << "AP::Type";
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
    out << (getKind() == Kind::ClangType ? "AP::ClangType("
                                         : "AP::ClangFunctionParamTupleType(");
    getType().dump(out);
    out << ", ";
    // It would be better to use print, but we need a PrintingPolicy
    // for that, for which we need a clang LangOptions, and... ugh.
    clang::QualType(getClangType(), 0).dump();
    out << ")";
    return;
  case Kind::ObjCMethodFormalParamTupleType:
  case Kind::ObjCMethodParamTupleType:
  case Kind::ObjCMethodType:
    out << (getKind() == Kind::ObjCMethodType
              ? "AP::ObjCMethodType(" :
            getKind() == Kind::ObjCMethodParamTupleType
              ? "AP::ObjCMethodParamTupleType("
              : "AP::ObjCMethodFormalParamTupleType(");
    getType().dump(out);
    out << ", ";
    getObjCMethod()->dump(out);
    out << ")";
    return;
  }
  llvm_unreachable("bad kind");
}

