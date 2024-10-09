//===--- Bridging/MiscBridging.cpp ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTBridging.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Assertions.h"

#ifdef PURE_BRIDGING_MODE
// In PURE_BRIDGING_MODE, bridging functions are not inlined and therefore
// included in the cpp file.
#include "swift/AST/ASTBridgingImpl.h"
#endif

using namespace swift;

//===----------------------------------------------------------------------===//
// MARK: Misc
//===----------------------------------------------------------------------===//

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"

void BridgedTopLevelCodeDecl_dump(void *decl) {
  static_cast<TopLevelCodeDecl *>(decl)->dump(llvm::errs());
}
void BridgedExpr_dump(void *expr) {
  static_cast<Expr *>(expr)->dump(llvm::errs());
}
void BridgedDecl_dump(void *decl) {
  static_cast<Decl *>(decl)->dump(llvm::errs());
}
void BridgedStmt_dump(void *statement) {
  static_cast<Stmt *>(statement)->dump(llvm::errs());
}
void BridgedTypeRepr_dump(void *type) { static_cast<TypeRepr *>(type)->dump(); }

#pragma clang diagnostic pop

//===----------------------------------------------------------------------===//
// MARK: Metatype registration
//===----------------------------------------------------------------------===//

static bool declMetatypesInitialized = false;

// Filled in by class registration in initializeSwiftModules().
static SwiftMetatype declMetatypes[(unsigned)DeclKind::Last_Decl + 1];

// Does return null if initializeSwiftModules() is never called.
SwiftMetatype Decl::getDeclMetatype(DeclKind kind) {
  SwiftMetatype metatype = declMetatypes[(unsigned)kind];
  if (declMetatypesInitialized && !metatype) {
    llvm::errs() << "Decl " << getKindName(kind) << " not registered\n";
    abort();
  }
  return metatype;
}

/// Registers the metatype of a Decl class.
/// Called by initializeSwiftModules().
void registerBridgedDecl(BridgedStringRef bridgedClassName,
                         SwiftMetatype metatype) {
  declMetatypesInitialized = true;

  auto declKind = llvm::StringSwitch<std::optional<swift::DeclKind>>(
                      bridgedClassName.unbridged())
#define DECL(Id, Parent) .Case(#Id "Decl", swift::DeclKind::Id)
#include "swift/AST/DeclNodes.def"
                      .Default(std::nullopt);

  if (!declKind) {
    llvm::errs() << "Unknown Decl class " << bridgedClassName.unbridged()
                 << "\n";
    abort();
  }
  declMetatypes[(unsigned)declKind.value()] = metatype;
}

BridgedOwnedString BridgedDeclObj::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  unbridged()->print(os);
  return str;
}

//===----------------------------------------------------------------------===//
// MARK: Conformance
//===----------------------------------------------------------------------===//

BridgedOwnedString BridgedConformance::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  unbridged().print(os);
  return str;
}

//===----------------------------------------------------------------------===//
// MARK: SubstitutionMap
//===----------------------------------------------------------------------===//

static_assert(sizeof(BridgedSubstitutionMap) >= sizeof(swift::SubstitutionMap),
              "BridgedSubstitutionMap has wrong size");

BridgedOwnedString BridgedSubstitutionMap::getDebugDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  unbridged().dump(os);
  return str;
}

