//===--- ASTBridgingImpl.h - header for the swift ASTBridging module ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ASTBRIDGINGIMPL_H
#define SWIFT_AST_ASTBRIDGINGIMPL_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ProtocolConformanceRef.h"

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

//===----------------------------------------------------------------------===//
// MARK: BridgedASTContext
//===----------------------------------------------------------------------===//

void * _Nonnull BridgedASTContext_raw(BridgedASTContext bridged) {
  return &bridged.unbridged();
}

BridgedASTContext BridgedASTContext_fromRaw(void * _Nonnull ptr) {
  return *static_cast<swift::ASTContext *>(ptr);
}

BRIDGED_INLINE
void *_Nullable BridgedASTContext_allocate(BridgedASTContext bridged,
                                           size_t size, size_t alignment) {
  return bridged.unbridged().Allocate(size, alignment);
}

BridgedStringRef BridgedASTContext_allocateCopyString(BridgedASTContext bridged,
                                                      BridgedStringRef cStr) {
  return bridged.unbridged().AllocateCopy(cStr.unbridged());
}

//===----------------------------------------------------------------------===//
// MARK: BridgedDeclObj
//===----------------------------------------------------------------------===//

BridgedSourceLoc BridgedDeclObj::getLoc() const {
  swift::SourceLoc sourceLoc = unbridged()->getLoc();
  return BridgedSourceLoc(sourceLoc.getOpaquePointerValue());
}

BridgedStringRef BridgedDeclObj::Type_getName() const {
  return getAs<swift::TypeDecl>()->getName().str();
}

BridgedStringRef BridgedDeclObj::Value_getUserFacingName() const {
  return getAs<swift::ValueDecl>()->getBaseName().userFacingName();
}

BridgedSourceLoc BridgedDeclObj::Value_getNameLoc() const {
  return BridgedSourceLoc(getAs<swift::ValueDecl>()->getNameLoc().getOpaquePointerValue());
}

bool BridgedDeclObj::Value_isObjC() const {
  return getAs<swift::ValueDecl>()->isObjC();
}

bool BridgedDeclObj::GenericType_isGenericAtAnyLevel() const {
  return getAs<swift::GenericTypeDecl>()->isGenericContext();
}

bool BridgedDeclObj::NominalType_isGlobalActor() const {
  return getAs<swift::NominalTypeDecl>()->isGlobalActor();
}

OptionalBridgedDeclObj BridgedDeclObj::NominalType_getValueTypeDestructor() const {
  return {getAs<swift::NominalTypeDecl>()->getValueTypeDestructor()};
}

bool BridgedDeclObj::Struct_hasUnreferenceableStorage() const {
  return getAs<swift::StructDecl>()->hasUnreferenceableStorage();
}

BridgedASTType BridgedDeclObj::Class_getSuperclass() const {
  return {getAs<swift::ClassDecl>()->getSuperclass().getPointer()};
}

//===----------------------------------------------------------------------===//
// MARK: BridgedSubscriptDecl
//===----------------------------------------------------------------------===//

BridgedAbstractStorageDecl
BridgedSubscriptDecl_asAbstractStorageDecl(BridgedSubscriptDecl decl) {
  return decl.unbridged();
}

//===----------------------------------------------------------------------===//
// MARK: BridgedVarDecl
//===----------------------------------------------------------------------===//

BridgedAbstractStorageDecl
BridgedVarDecl_asAbstractStorageDecl(BridgedVarDecl decl) {
  return decl.unbridged();
}

//===----------------------------------------------------------------------===//
// MARK: BridgedASTType
//===----------------------------------------------------------------------===//

swift::Type BridgedASTType::unbridged() const {
  return type;
}

BridgedOwnedString BridgedASTType::getDebugDescription() const {
  return BridgedOwnedString(unbridged().getString());
}

BridgedCanType BridgedASTType::getCanonicalType() const {
  return unbridged()->getCanonicalType();
}

bool BridgedASTType::hasTypeParameter() const {
  return unbridged()->hasTypeParameter();
}

bool BridgedASTType::isOpenedExistentialWithError() const {
  return unbridged()->isOpenedExistentialWithError();
}

bool BridgedASTType::isEscapable() const {
  return unbridged()->isEscapable();
}

bool BridgedASTType::isNoEscape() const {
  return unbridged()->isNoEscape();
}

bool BridgedASTType::isInteger() const {
  return unbridged()->is<swift::IntegerType>();
}

BridgedASTType BridgedASTType::subst(BridgedSubstitutionMap substMap) const {
  return {unbridged().subst(substMap.unbridged()).getPointer()};
}

//===----------------------------------------------------------------------===//
// MARK: BridgedCanType
//===----------------------------------------------------------------------===//

BridgedCanType::BridgedCanType(swift::CanType ty) : type(ty.getPointer()) {
}

swift::CanType BridgedCanType::unbridged() const {
  return swift::CanType(type);
}

BridgedASTType BridgedCanType::getType() const {
  return {type};
}

//===----------------------------------------------------------------------===//
// MARK: BridgedConformance
//===----------------------------------------------------------------------===//

static_assert(sizeof(BridgedConformance) == sizeof(swift::ProtocolConformanceRef));

bool BridgedConformance::isConcrete() const {
  return unbridged().isConcrete();
}

bool BridgedConformance::isValid() const {
  return !unbridged().isInvalid();
}

bool BridgedConformance::isSpecializedConformance() const {
  return swift::isa<swift::SpecializedProtocolConformance>(unbridged().getConcrete());
}

bool BridgedConformance::isInheritedConformance() const {
  return swift::isa<swift::InheritedProtocolConformance>(unbridged().getConcrete());
}

BridgedASTType BridgedConformance::getType() const {
  return {unbridged().getConcrete()->getType().getPointer()};
}

BridgedConformance BridgedConformance::getGenericConformance() const {
  auto *specPC = swift::cast<swift::SpecializedProtocolConformance>(unbridged().getConcrete());
  return {swift::ProtocolConformanceRef(specPC->getGenericConformance())};
}

BridgedConformance BridgedConformance::getInheritedConformance() const {
  auto *inheritedConf = swift::cast<swift::InheritedProtocolConformance>(unbridged().getConcrete());
  return {swift::ProtocolConformanceRef(inheritedConf->getInheritedConformance())};
}

BridgedSubstitutionMap BridgedConformance::getSpecializedSubstitutions() const {
  auto *specPC = swift::cast<swift::SpecializedProtocolConformance>(unbridged().getConcrete());
  return {specPC->getSubstitutionMap()};
}

BridgedConformance BridgedConformanceArray::getAt(SwiftInt index) const {
  return unbridged()[index];
}

//===----------------------------------------------------------------------===//
// MARK: BridgedSubstitutionMap
//===----------------------------------------------------------------------===//

BridgedSubstitutionMap::BridgedSubstitutionMap(swift::SubstitutionMap map) {
  *reinterpret_cast<swift::SubstitutionMap *>(&storage) = map;
}

swift::SubstitutionMap BridgedSubstitutionMap::unbridged() const {
  return *reinterpret_cast<const swift::SubstitutionMap *>(&storage);
}

BridgedSubstitutionMap::BridgedSubstitutionMap() : BridgedSubstitutionMap(swift::SubstitutionMap()) {
}

bool BridgedSubstitutionMap::isEmpty() const {
  return unbridged().empty();
}

bool BridgedSubstitutionMap::hasAnySubstitutableParams() const {
  return unbridged().hasAnySubstitutableParams();
}

SwiftInt BridgedSubstitutionMap::getNumConformances() const {
  return (SwiftInt)unbridged().getConformances().size();
}

BridgedConformance BridgedSubstitutionMap::getConformance(SwiftInt index) const {
  return unbridged().getConformances()[index];
}

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif // SWIFT_AST_ASTBRIDGINGIMPL_H
