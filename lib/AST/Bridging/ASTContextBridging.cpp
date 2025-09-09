//===--- Bridging/ASTContextBridging.cpp ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTBridging.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/AvailabilitySpec.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// MARK: ASTContext
//===----------------------------------------------------------------------===//

Identifier BridgedASTContext_getIdentifier(BridgedASTContext cContext,
                                           BridgedStringRef cStr) {
  return cContext.unbridged().getIdentifier(cStr.unbridged());
}

Identifier BridgedASTContext__getIdentifier(BridgedASTContext cContext,
                                            BridgedStringRef cStr) {
  return cContext.unbridged().getIdentifier(cStr.unbridged());
}

Identifier BridgedASTContext_getDollarIdentifier(BridgedASTContext cContext,
                                                 size_t idx) {
  return cContext.unbridged().getDollarIdentifier(idx);
}

bool BridgedASTContext_langOptsHasFeature(BridgedASTContext cContext,
                                          BridgedFeature feature) {
  return cContext.unbridged().LangOpts.hasFeature((Feature)feature);
}

unsigned BridgedASTContext::getMajorLanguageVersion() const {
  return unbridged().LangOpts.EffectiveLanguageVersion[0];
}

bool BridgedASTContext_langOptsCustomConditionSet(BridgedASTContext cContext,
                                                  BridgedStringRef cName) {
  ASTContext &ctx = cContext.unbridged();
  auto name = cName.unbridged();
  if (name.starts_with("$") && ctx.LangOpts.hasFeature(name.drop_front()))
    return true;

  return ctx.LangOpts.isCustomConditionalCompilationFlagSet(name);
}

bool BridgedASTContext_langOptsHasFeatureNamed(BridgedASTContext cContext,
                                               BridgedStringRef cName) {
  return cContext.unbridged().LangOpts.hasFeature(cName.unbridged());
}

bool BridgedASTContext_langOptsHasAttributeNamed(BridgedASTContext cContext,
                                                 BridgedStringRef cName) {
  return hasAttribute(cContext.unbridged().LangOpts, cName.unbridged());
}

bool BridgedASTContext_langOptsIsActiveTargetOS(BridgedASTContext cContext,
                                                BridgedStringRef cName) {
  return cContext.unbridged().LangOpts.checkPlatformCondition(
      PlatformConditionKind::OS, cName.unbridged());
}

bool BridgedASTContext_langOptsIsActiveTargetArchitecture(
    BridgedASTContext cContext, BridgedStringRef cName) {
  return cContext.unbridged().LangOpts.checkPlatformCondition(
      PlatformConditionKind::Arch, cName.unbridged());
}

bool BridgedASTContext_langOptsIsActiveTargetEnvironment(
    BridgedASTContext cContext, BridgedStringRef cName) {
  return cContext.unbridged().LangOpts.checkPlatformCondition(
      PlatformConditionKind::TargetEnvironment, cName.unbridged());
}

bool BridgedASTContext_langOptsIsActiveTargetRuntime(BridgedASTContext cContext,
                                                     BridgedStringRef cName) {
  return cContext.unbridged().LangOpts.checkPlatformCondition(
      PlatformConditionKind::Runtime, cName.unbridged());
}

bool BridgedASTContext_langOptsIsActiveTargetPtrAuth(BridgedASTContext cContext,
                                                     BridgedStringRef cName) {
  return cContext.unbridged().LangOpts.checkPlatformCondition(
      PlatformConditionKind::PtrAuth, cName.unbridged());
}

unsigned BridgedASTContext::getLangOptsTargetPointerBitWidth() const {
  return unbridged().LangOpts.Target.isArch64Bit()   ? 64
         : unbridged().LangOpts.Target.isArch32Bit() ? 32
         : unbridged().LangOpts.Target.isArch16Bit() ? 16
                                                     : 0;
}

bool BridgedASTContext::getLangOptsAttachCommentsToDecls() const {
  return unbridged().LangOpts.AttachCommentsToDecls;
}

BridgedEndianness BridgedASTContext::getLangOptsTargetEndianness() const {
  return unbridged().LangOpts.Target.isLittleEndian() ? EndianLittle
                                                      : EndianBig;
}

/// Convert an array of numbers into a form we can use in Swift.
namespace {
template <typename Arr>
SwiftInt convertArray(const Arr &array, SwiftInt **cElements) {
  SwiftInt numElements = array.size();
  *cElements = (SwiftInt *)malloc(sizeof(SwiftInt) * numElements);
  for (SwiftInt i = 0; i != numElements; ++i)
    (*cElements)[i] = array[i];
  return numElements;
}
} // namespace

void deallocateIntBuffer(SwiftInt *_Nullable cComponents) { free(cComponents); }

SwiftInt
BridgedASTContext_langOptsGetLanguageVersion(BridgedASTContext cContext,
                                             SwiftInt **cComponents) {
  auto theVersion = cContext.unbridged().LangOpts.EffectiveLanguageVersion;
  return convertArray(theVersion, cComponents);
}

SWIFT_NAME("BridgedASTContext.langOptsGetCompilerVersion(self:_:)")
SwiftInt
BridgedASTContext_langOptsGetCompilerVersion(BridgedASTContext cContext,
                                             SwiftInt **cComponents) {
  auto theVersion = version::Version::getCurrentLanguageVersion();
  return convertArray(theVersion, cComponents);
}

SwiftInt BridgedASTContext_langOptsGetTargetAtomicBitWidths(
    BridgedASTContext cContext, SwiftInt *_Nullable *_Nonnull cElements) {
  return convertArray(cContext.unbridged().LangOpts.getAtomicBitWidthValues(),
                      cElements);
}

bool BridgedASTContext_canImport(BridgedASTContext cContext,
                                 BridgedStringRef importPath,
                                 SourceLoc canImportLoc,
                                 BridgedCanImportVersion versionKind,
                                 const SwiftInt *_Nullable versionComponents,
                                 SwiftInt numVersionComponents) {
  // Map the version.
  llvm::VersionTuple version;
  switch (numVersionComponents) {
  case 0:
    break;
  case 1:
    version = llvm::VersionTuple(versionComponents[0]);
    break;
  case 2:
    version = llvm::VersionTuple(versionComponents[0], versionComponents[1]);
    break;
  case 3:
    version = llvm::VersionTuple(versionComponents[0], versionComponents[1],
                                 versionComponents[2]);
    break;
  default:
    version = llvm::VersionTuple(versionComponents[0], versionComponents[1],
                                 versionComponents[2], versionComponents[3]);
    break;
  }

  ImportPath::Module::Builder builder(cContext.unbridged(),
                                      importPath.unbridged(), /*separator=*/'.',
                                      canImportLoc);
  return cContext.unbridged().canImportModule(
      builder.get(), canImportLoc, version,
      versionKind == CanImportUnderlyingVersion);
}

BridgedAvailabilityMacroMap BridgedASTContext::getAvailabilityMacroMap() const {
  return &unbridged().getAvailabilityMacroMap();
}
