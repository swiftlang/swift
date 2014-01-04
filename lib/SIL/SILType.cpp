//===--- SILType.cpp - Defines SILType ------------------------------------===//
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

#include "swift/SIL/SILType.h"
#include "swift/AST/Type.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/AbstractionPattern.h"

using namespace swift;
using namespace swift::Lowering;

SILType SILType::getObjectPointerType(const ASTContext &C) {
  return SILType(CanType(C.TheObjectPointerType), SILValueCategory::Object);
}

SILType SILType::getObjCPointerType(const ASTContext &C) {
  return getPrimitiveObjectType(CanType(C.TheObjCPointerType));
}

SILType SILType::getRawPointerType(const ASTContext &C) {
  return getPrimitiveObjectType(CanType(C.TheRawPointerType));
}

SILType SILType::getBuiltinIntegerType(unsigned bitWidth,
                                       const ASTContext &C) {
  return getPrimitiveObjectType(CanType(BuiltinIntegerType::get(bitWidth, C)));
}

SILType SILType::getBuiltinFloatType(BuiltinFloatType::FPKind Kind,
                                     const ASTContext &C) {
  Type ty;
  switch (Kind) {
  case BuiltinFloatType::IEEE16:  ty = C.TheIEEE16Type; break;
  case BuiltinFloatType::IEEE32:  ty = C.TheIEEE32Type; break;
  case BuiltinFloatType::IEEE64:  ty = C.TheIEEE64Type; break;
  case BuiltinFloatType::IEEE80:  ty = C.TheIEEE80Type; break;
  case BuiltinFloatType::IEEE128: ty = C.TheIEEE128Type; break;
  case BuiltinFloatType::PPC128:  ty = C.ThePPC128Type; break;
  }
  return getPrimitiveObjectType(CanType(ty));
}

SILType SILType::getBuiltinWordType(const ASTContext &C) {
  return getPrimitiveObjectType(CanType(BuiltinIntegerType::getWordType(C)));
}

bool SILType::isTrivial(SILModule &M) const {
  return M.getTypeLowering(*this).isTrivial();
}

std::string SILType::getAsString() const {
  std::string Result;
  llvm::raw_string_ostream OS(Result);
  print(OS);
  return OS.str();
}

SILType SILType::getFieldType(VarDecl *field, SILModule &M) const {
  assert(field->getDeclContext() == getNominalOrBoundGenericNominal());
  auto origFieldTy = AbstractionPattern(field->getType());
  auto substFieldTy =
    getSwiftRValueType()->getTypeOfMember(M.getSwiftModule(),
                                          field, nullptr);
  auto loweredTy = M.Types.getLoweredType(origFieldTy, substFieldTy);
  if (isAddress() || getClassOrBoundGenericClass() != nullptr) {
    return loweredTy.getAddressType();
  } else {
    return loweredTy.getObjectType();
  }
}

SILType SILType::getEnumElementType(EnumElementDecl *elt, SILModule &M) const {
  assert(elt->getDeclContext() == getEnumOrBoundGenericEnum());
  assert(elt->hasArgumentType());
  auto origEltTy = elt->getArgumentType();
  auto substEltTy =
    getSwiftRValueType()->getTypeOfMember(M.getSwiftModule(),
                                          elt, nullptr, origEltTy);
  auto loweredTy =
    M.Types.getLoweredType(AbstractionPattern(origEltTy), substEltTy);
  return SILType(loweredTy.getSwiftRValueType(), getCategory());
}
