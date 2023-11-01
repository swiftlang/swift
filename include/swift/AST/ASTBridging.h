//===--- ASTBridging.h - header for the swift SILBridging module ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ASTBRIDGING_H
#define SWIFT_AST_ASTBRIDGING_H

// Do not add other C++/llvm/swift header files here!
// Function implementations should be placed into ASTBridging.cpp and required header files should be added there.
//
#include "swift/Basic/BasicBridging.h"

#ifdef USED_IN_CPP_SOURCE
#include "swift/AST/DiagnosticConsumer.h"
#include "swift/AST/DiagnosticEngine.h"
#endif

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

namespace swift {
class ASTContext;
class DiagnosticArgument;
class DiagnosticEngine;
}

//===----------------------------------------------------------------------===//
// MARK: Identifier
//===----------------------------------------------------------------------===//

class BridgedIdentifier {
public:
  SWIFT_UNAVAILABLE("Use '.raw' instead")
  const void *_Nullable Raw;

  BridgedIdentifier() : Raw(nullptr) {}

  SWIFT_NAME("init(raw:)")
  BridgedIdentifier(const void *_Nullable raw) : Raw(raw) {}

#ifdef USED_IN_CPP_SOURCE
  BridgedIdentifier(swift::Identifier ident)
      : Raw(ident.getAsOpaquePointer()) {}

  swift::Identifier unbridged() const {
    return swift::Identifier::getFromOpaquePointer(Raw);
  }
#endif
};

SWIFT_NAME("getter:BridgedIdentifier.raw(self:)")
inline const void *_Nullable BridgedIdentifier_raw(BridgedIdentifier ident) {
  return ident.Raw;
}

struct BridgedIdentifierAndSourceLoc {
  SWIFT_NAME("name")
  BridgedIdentifier Name;

  SWIFT_NAME("nameLoc")
  BridgedSourceLoc NameLoc;
};

//===----------------------------------------------------------------------===//
// MARK: ASTContext
//===----------------------------------------------------------------------===//

class BridgedASTContext {
  swift::ASTContext * _Nonnull Ctx;

public:
#ifdef USED_IN_CPP_SOURCE
  SWIFT_UNAVAILABLE("Use init(raw:) instead")
  BridgedASTContext(swift::ASTContext &ctx) : Ctx(&ctx) {}

  SWIFT_UNAVAILABLE("Use '.raw' instead")
  swift::ASTContext &unbridged() const { return *Ctx; }
#endif
};

SWIFT_NAME("getter:BridgedASTContext.raw(self:)")
BRIDGED_INLINE
void * _Nonnull BridgedASTContext_raw(BridgedASTContext bridged);

SWIFT_NAME("BridgedASTContext.init(raw:)")
BRIDGED_INLINE
BridgedASTContext BridgedASTContext_fromRaw(void * _Nonnull ptr);

SWIFT_NAME("BridgedASTContext.getIdentifier(self:_:)")
BridgedIdentifier BridgedASTContext_getIdentifier(BridgedASTContext cContext,
                                                  BridgedStringRef cStr);

SWIFT_NAME("BridgedASTContext.langOptsHasFeature(self:_:)")
bool BridgedASTContext_langOptsHasFeature(BridgedASTContext cContext,
                                          BridgedFeature feature);

//===----------------------------------------------------------------------===//
// MARK: AST nodes
//===----------------------------------------------------------------------===//

enum ENUM_EXTENSIBILITY_ATTR(open) ASTNodeKind : size_t {
  ASTNodeKindExpr,
  ASTNodeKindStmt,
  ASTNodeKindDecl
};

struct BridgedASTNode {
  SWIFT_NAME("raw")
  void *_Nonnull Raw;

  SWIFT_NAME("kind")
  ASTNodeKind Kind;
};

// Forward declare the underlying AST node type for each wrapper.
namespace swift {
#define AST_BRIDGING_WRAPPER(Name) class Name;
#include "swift/AST/ASTBridgingWrappers.def"
} // end namespace swift

// Define the bridging wrappers for each AST node.
#define AST_BRIDGING_WRAPPER(Name) BRIDGING_WRAPPER_NONNULL(swift::Name, Name)
#include "swift/AST/ASTBridgingWrappers.def"

// For nullable nodes, also define a nullable variant.
#define AST_BRIDGING_WRAPPER_NULLABLE(Name)                                    \
  BRIDGING_WRAPPER_NULLABLE(swift::Name, Name)
#define AST_BRIDGING_WRAPPER_NONNULL(Name)
#include "swift/AST/ASTBridgingWrappers.def"

// Declare `.asDecl` on each BridgedXXXDecl type, which upcasts a wrapper for
// a Decl subclass to a BridgedDecl.
#define DECL(Id, Parent)                                                       \
  SWIFT_NAME("getter:Bridged" #Id "Decl.asDecl(self:)")                        \
  BridgedDecl Bridged##Id##Decl_asDecl(Bridged##Id##Decl decl);
#define ABSTRACT_DECL(Id, Parent) DECL(Id, Parent)
#include "swift/AST/DeclNodes.def"

// Declare `.asDeclContext` on each BridgedXXXDecl type that's also a
// DeclContext.
#define DECL(Id, Parent)
#define CONTEXT_DECL(Id, Parent)                                               \
  SWIFT_NAME("getter:Bridged" #Id "Decl.asDeclContext(self:)")                 \
  BridgedDeclContext Bridged##Id##Decl_asDeclContext(Bridged##Id##Decl decl);
#define ABSTRACT_CONTEXT_DECL(Id, Parent) CONTEXT_DECL(Id, Parent)
#include "swift/AST/DeclNodes.def"

// Declare `.asStmt` on each BridgedXXXStmt type, which upcasts a wrapper for
// a Stmt subclass to a BridgedStmt.
#define STMT(Id, Parent)                                                       \
  SWIFT_NAME("getter:Bridged" #Id "Stmt.asStmt(self:)")                        \
  BridgedStmt Bridged##Id##Stmt_asStmt(Bridged##Id##Stmt stmt);
#define ABSTRACT_STMT(Id, Parent) STMT(Id, Parent)
#include "swift/AST/StmtNodes.def"

// Declare `.asExpr` on each BridgedXXXExpr type, which upcasts a wrapper for
// a Expr subclass to a BridgedExpr.
#define EXPR(Id, Parent)                                                       \
  SWIFT_NAME("getter:Bridged" #Id "Expr.asExpr(self:)")                        \
  BridgedExpr Bridged##Id##Expr_asExpr(Bridged##Id##Expr expr);
#define ABSTRACT_EXPR(Id, Parent) EXPR(Id, Parent)
#include "swift/AST/ExprNodes.def"

// Declare `.asTypeRepr` on each BridgedXXXTypeRepr type, which upcasts a
// wrapper for a TypeRepr subclass to a BridgedTypeRepr.
#define TYPEREPR(Id, Parent)                                                   \
  SWIFT_NAME("getter:Bridged" #Id "TypeRepr.asTypeRepr(self:)")                \
  BridgedTypeRepr Bridged##Id##TypeRepr_asTypeRepr(                            \
      Bridged##Id##TypeRepr typeRepr);
#define ABSTRACT_TYPEREPR(Id, Parent) TYPEREPR(Id, Parent)
#include "swift/AST/TypeReprNodes.def"

//===----------------------------------------------------------------------===//
// MARK: Diagnostic Engine
//===----------------------------------------------------------------------===//

// NOTE: This must be the same underlying value as C++ 'swift::DiagID' defined
// in 'DiagnosticList.cpp'.
enum ENUM_EXTENSIBILITY_ATTR(open) BridgedDiagID : uint32_t {
#define DIAG(KIND, ID, Options, Text, Signature) BridgedDiagID_##ID,
#include "swift/AST/DiagnosticsAll.def"
};

class BridgedDiagnosticArgument {
  int64_t storage[3];

public:
#ifdef USED_IN_CPP_SOURCE
  BridgedDiagnosticArgument(const swift::DiagnosticArgument &arg) {
    *reinterpret_cast<swift::DiagnosticArgument *>(&storage) = arg;
  }
  const swift::DiagnosticArgument &unbridged() const {
    return *reinterpret_cast<const swift::DiagnosticArgument *>(&storage);
  }
#endif

  BridgedDiagnosticArgument(SwiftInt i);
  BridgedDiagnosticArgument(BridgedStringRef s);
};

class BridgedDiagnosticFixIt {
  int64_t storage[7];

public:
#ifdef USED_IN_CPP_SOURCE
  BridgedDiagnosticFixIt(const swift::DiagnosticInfo::FixIt &fixit){
    *reinterpret_cast<swift::DiagnosticInfo::FixIt *>(&storage) = fixit;
  }
  const swift::DiagnosticInfo::FixIt &unbridged() const {
    return *reinterpret_cast<const swift::DiagnosticInfo::FixIt *>(&storage);
  }
#endif

  BridgedDiagnosticFixIt(BridgedSourceLoc start, uint32_t length, BridgedStringRef text);
};

/// Diagnostic severity when reporting diagnostics.
enum ENUM_EXTENSIBILITY_ATTR(open) BridgedDiagnosticSeverity : size_t {
  BridgedFatalError,
  BridgedError,
  BridgedWarning,
  BridgedRemark,
  BridgedNote,
};

class BridgedDiagnostic {
public:
  struct Impl;

  SWIFT_UNAVAILABLE("Unavailable in Swift")
  Impl *_Nonnull Raw;

  SWIFT_UNAVAILABLE("Unavailable in Swift")
  BridgedDiagnostic(Impl *_Nonnull raw) : Raw(raw) {}

  SWIFT_UNAVAILABLE("Unavailable in Swift")
  Impl *_Nonnull unbridged() const { return Raw; }
};

// FIXME: Can we bridge InFlightDiagnostic?
SWIFT_NAME("BridgedDiagnosticEngine.diagnose(self:at:_:_:highlightAt:"
           "highlightLength:fixIts:)")
void BridgedDiagnosticEngine_diagnose(
    BridgedDiagnosticEngine, BridgedSourceLoc loc, BridgedDiagID diagID,
    BridgedArrayRef arguments, BridgedSourceLoc highlightStart,
    uint32_t hightlightLength, BridgedArrayRef fixIts);

SWIFT_NAME("getter:BridgedDiagnosticEngine.hadAnyError(self:)")
bool BridgedDiagnosticEngine_hadAnyError(BridgedDiagnosticEngine);

/// Create a new diagnostic with the given severity, location, and diagnostic
/// text.
///
/// \returns a diagnostic instance that can be extended with additional
/// information and then must be finished via \c SwiftDiagnostic_finish.
SWIFT_NAME("BridgedDiagnostic.init(at:message:severity:engine:)")
BridgedDiagnostic BridgedDiagnostic_create(BridgedSourceLoc cLoc,
                                           BridgedStringRef cText,
                                           BridgedDiagnosticSeverity severity,
                                           BridgedDiagnosticEngine cDiags);

/// Highlight a source range as part of the diagnostic.
SWIFT_NAME("BridgedDiagnostic.highlight(self:start:end:)")
void BridgedDiagnostic_highlight(BridgedDiagnostic cDiag,
                                 BridgedSourceLoc cStartLoc,
                                 BridgedSourceLoc cEndLoc);

/// Add a Fix-It to replace a source range as part of the diagnostic.
SWIFT_NAME("BridgedDiagnostic.fixItReplace(self:start:end:replacement:)")
void BridgedDiagnostic_fixItReplace(BridgedDiagnostic cDiag,
                                    BridgedSourceLoc cStartLoc,
                                    BridgedSourceLoc cEndLoc,
                                    BridgedStringRef cReplaceText);

/// Finish the given diagnostic and emit it.
SWIFT_NAME("BridgedDiagnostic.finish(self:)")
void BridgedDiagnostic_finish(BridgedDiagnostic cDiag);

//===----------------------------------------------------------------------===//
// MARK: Decls
//===----------------------------------------------------------------------===//

SWIFT_NAME(
    "BridgedPatternBindingDecl.createParsed(_:declContext:bindingKeywordLoc:"
    "nameExpr:initializer:isStatic:isLet:)")
BridgedPatternBindingDecl BridgedPatternBindingDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cBindingKeywordLoc, BridgedExpr opaqueNameExpr,
    BridgedExpr opaqueInitExpr, bool isStatic, bool isLet);

SWIFT_NAME("BridgedParamDecl.createParsed(_:declContext:specifierLoc:firstName:"
           "firstNameLoc:secondName:secondNameLoc:type:defaultValue:)")
BridgedParamDecl BridgedParamDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cSpecifierLoc, BridgedIdentifier cFirstName,
    BridgedSourceLoc cFirstNameLoc, BridgedIdentifier cSecondName,
    BridgedSourceLoc cSecondNameLoc, BridgedNullableTypeRepr type,
    BridgedNullableExpr defaultValue);

SWIFT_NAME("BridgedConstructorDecl.setParsedBody(self:_:)")
void BridgedConstructorDecl_setParsedBody(BridgedConstructorDecl decl,
                                          BridgedBraceStmt body);

SWIFT_NAME("BridgedFuncDecl.setParsedBody(self:_:)")
void BridgedFuncDecl_setParsedBody(BridgedFuncDecl decl, BridgedBraceStmt body);

SWIFT_NAME("BridgedDestructorDecl.setParsedBody(self:_:)")
void BridgedDestructorDecl_setParsedBody(BridgedDestructorDecl decl,
                                         BridgedBraceStmt body);

SWIFT_NAME(
    "BridgedFuncDecl.createParsed(_:declContext:staticLoc:funcKeywordLoc:"
    "name:nameLoc:genericParamList:parameterList:asyncSpecifierLoc:"
    "throwsSpecifierLoc:thrownType:returnType:genericWhereClause:)")
BridgedFuncDecl BridgedFuncDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStaticLoc, BridgedSourceLoc cFuncKeywordLoc,
    BridgedIdentifier cName, BridgedSourceLoc cNameLoc,
    BridgedNullableGenericParamList genericParamList,
    BridgedParameterList parameterList, BridgedSourceLoc cAsyncLoc,
    BridgedSourceLoc cThrowsLoc, BridgedNullableTypeRepr thrownType,
    BridgedNullableTypeRepr returnType,
    BridgedNullableTrailingWhereClause opaqueGenericWhereClause);

SWIFT_NAME(
    "BridgedConstructorDecl.createParsed(_:declContext:initKeywordLoc:"
    "failabilityMarkLoc:isIUO:genericParamList:parameterList:"
    "asyncSpecifierLoc:throwsSpecifierLoc:thrownType:genericWhereClause:)")
BridgedConstructorDecl BridgedConstructorDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cInitKeywordLoc, BridgedSourceLoc cFailabilityMarkLoc,
    bool isIUO, BridgedNullableGenericParamList genericParams,
    BridgedParameterList parameterList, BridgedSourceLoc cAsyncLoc,
    BridgedSourceLoc cThrowsLoc, BridgedNullableTypeRepr thrownType,
    BridgedNullableTrailingWhereClause genericWhereClause);

SWIFT_NAME(
    "BridgedDestructorDecl.createParsed(_:declContext:deinitKeywordLoc:)")
BridgedDestructorDecl
BridgedDestructorDecl_createParsed(BridgedASTContext cContext,
                                   BridgedDeclContext cDeclContext,
                                   BridgedSourceLoc cDeinitKeywordLoc);

SWIFT_NAME(
    "BridgedTypeAliasDecl.createParsed(_:declContext:typealiasKeywordLoc:name:"
    "nameLoc:genericParamList:equalLoc:underlyingType:genericWhereClause:)")
BridgedTypeAliasDecl BridgedTypeAliasDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cAliasKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedSourceLoc cEqualLoc, BridgedTypeRepr underlyingType,
    BridgedNullableTrailingWhereClause genericWhereClause);

SWIFT_NAME("BridgedExtensionDecl.setParsedMembers(self:_:)")
void BridgedExtensionDecl_setParsedMembers(BridgedExtensionDecl decl,
                                           BridgedArrayRef members);

SWIFT_NAME(
    "BridgedEnumDecl.createParsed(_:declContext:enumKeywordLoc:name:nameLoc:"
    "genericParamList:inheritedTypes:genericWhereClause:braceRange:)")
BridgedNominalTypeDecl BridgedEnumDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cEnumKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange);

SWIFT_NAME(
    "BridgedEnumCaseDecl.createParsed(declContext:caseKeywordLoc:elements:)")
BridgedEnumCaseDecl
BridgedEnumCaseDecl_createParsed(BridgedDeclContext cDeclContext,
                                 BridgedSourceLoc cCaseKeywordLoc,
                                 BridgedArrayRef cElements);

SWIFT_NAME("BridgedEnumElementDecl.createParsed(_:declContext:name:nameLoc:"
           "parameterList:equalsLoc:rawValue:)")
BridgedEnumElementDecl BridgedEnumElementDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedIdentifier cName, BridgedSourceLoc cNameLoc,
    BridgedNullableParameterList parameterList, BridgedSourceLoc cEqualsLoc,
    BridgedNullableExpr opaqueRawValue);

SWIFT_NAME("BridgedStructDecl.createParsed(_:declContext:structKeywordLoc:name:"
           "nameLoc:genericParamList:inheritedTypes:genericWhereClause:"
           "braceRange:)")
BridgedNominalTypeDecl BridgedStructDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStructKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange);

SWIFT_NAME(
    "BridgedClassDecl.createParsed(_:declContext:classKeywordLoc:name:nameLoc:"
    "genericParamList:inheritedTypes:genericWhereClause:braceRange:isActor:)")
BridgedNominalTypeDecl BridgedClassDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cClassKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange, bool isActor);

SWIFT_NAME(
    "BridgedProtocolDecl.createParsed(_:declContext:protocolKeywordLoc:name:"
    "nameLoc:primaryAssociatedTypeNames:inheritedTypes:"
    "genericWhereClause:braceRange:)")
BridgedNominalTypeDecl BridgedProtocolDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cProtocolKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedArrayRef cPrimaryAssociatedTypeNames,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange);

SWIFT_NAME("BridgedAssociatedTypeDecl.createParsed(_:declContext:"
           "associatedtypeKeywordLoc:name:nameLoc:inheritedTypes:defaultType:"
           "genericWhereClause:)")
BridgedAssociatedTypeDecl BridgedAssociatedTypeDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cAssociatedtypeKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedArrayRef cInheritedTypes,
    BridgedNullableTypeRepr opaqueDefaultType,
    BridgedNullableTrailingWhereClause genericWhereClause);

SWIFT_NAME(
    "BridgedExtensionDecl.createParsed(_:declContext:extensionKeywordLoc:"
    "extendedType:inheritedTypes:genericWhereClause:braceRange:)")
BridgedExtensionDecl BridgedExtensionDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cExtensionKeywordLoc, BridgedTypeRepr opaqueExtendedType,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedOperatorFixity {
  BridgedOperatorFixityInfix,
  BridgedOperatorFixityPrefix,
  BridgedOperatorFixityPostfix,
};

SWIFT_NAME("BridgedOperatorDecl.createParsed(_:declContext:fixity:"
           "operatorKeywordLoc:name:nameLoc:colonLoc:precedenceGroupName:"
           "precedenceGroupLoc:)")
BridgedOperatorDecl BridgedOperatorDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedOperatorFixity cFixity, BridgedSourceLoc cOperatorKeywordLoc,
    BridgedIdentifier cName, BridgedSourceLoc cNameLoc,
    BridgedSourceLoc cColonLoc, BridgedIdentifier cPrecedenceGroupName,
    BridgedSourceLoc cPrecedenceGroupLoc);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedAssociativity {
  BridgedAssociativityNone,
  BridgedAssociativityLeft,
  BridgedAssociativityRight,
};

SWIFT_NAME("BridgedPrecedenceGroupDecl.createParsed(declContext:"
           "precedencegroupKeywordLoc:name:nameLoc:leftBraceLoc:"
           "associativityLabelLoc:associativityValueLoc:associativity:"
           "assignmentLabelLoc:assignmentValueLoc:isAssignment:"
           "higherThanKeywordLoc:higherThanNames:lowerThanKeywordLoc:"
           "lowerThanNames:rightBraceLoc:)")
BridgedPrecedenceGroupDecl BridgedPrecedenceGroupDecl_createParsed(
    BridgedDeclContext cDeclContext,
    BridgedSourceLoc cPrecedencegroupKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedSourceLoc cLeftBraceLoc,
    BridgedSourceLoc cAssociativityKeywordLoc,
    BridgedSourceLoc cAssociativityValueLoc,
    BridgedAssociativity cAssociativity, BridgedSourceLoc cAssignmentKeywordLoc,
    BridgedSourceLoc cAssignmentValueLoc, bool isAssignment,
    BridgedSourceLoc cHigherThanKeywordLoc, BridgedArrayRef cHigherThanNames,
    BridgedSourceLoc cLowerThanKeywordLoc, BridgedArrayRef cLowerThanNames,
    BridgedSourceLoc cRightBraceLoc);

enum ENUM_EXTENSIBILITY_ATTR(open) BridgedImportKind {
  BridgedImportKindModule,
  BridgedImportKindType,
  BridgedImportKindStruct,
  BridgedImportKindClass,
  BridgedImportKindEnum,
  BridgedImportKindProtocol,
  BridgedImportKindVar,
  BridgedImportKindFunc,
};

SWIFT_NAME("BridgedImportDecl.createParsed(_:declContext:importKeywordLoc:"
           "importKind:importKindLoc:path:)")
BridgedImportDecl BridgedImportDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cImportKeywordLoc, BridgedImportKind cImportKind,
    BridgedSourceLoc cImportKindLoc, BridgedArrayRef cImportPathElements);

SWIFT_NAME(
    "BridgedTopLevelCodeDecl.createParsed(_:declContext:startLoc:stmt:endLoc:)")
BridgedTopLevelCodeDecl BridgedTopLevelCodeDecl_createStmt(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStartLoc, BridgedStmt statement,
    BridgedSourceLoc cEndLoc);

SWIFT_NAME(
    "BridgedTopLevelCodeDecl.createParsed(_:declContext:startLoc:expr:endLoc:)")
BridgedTopLevelCodeDecl BridgedTopLevelCodeDecl_createExpr(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStartLoc, BridgedExpr expression,
    BridgedSourceLoc cEndLoc);

SWIFT_NAME("BridgedTopLevelCodeDecl.dump(self:)")
void BridgedTopLevelCodeDecl_dump(BridgedTopLevelCodeDecl decl);

SWIFT_NAME("BridgedDecl.dump(self:)")
void BridgedDecl_dump(BridgedDecl decl);

//===----------------------------------------------------------------------===//
// MARK: NominalTypeDecl
//===----------------------------------------------------------------------===//

SWIFT_NAME("BridgedNominalTypeDecl.getName(self:)")
BRIDGED_INLINE
BridgedStringRef BridgedNominalTypeDecl_getName(BridgedNominalTypeDecl decl);

SWIFT_NAME("BridgedNominalTypeDecl.isStructWithUnreferenceableStorage(self:)")
bool BridgedNominalTypeDecl_isStructWithUnreferenceableStorage(
    BridgedNominalTypeDecl decl);

SWIFT_NAME("BridgedNominalTypeDecl.isGlobalActor(self:)")
BRIDGED_INLINE
bool BridgedNominalTypeDecl_isGlobalActor(BridgedNominalTypeDecl decl);

SWIFT_NAME("BridgedNominalTypeDecl.setParsedMembers(self:_:)")
void BridgedNominalTypeDecl_setParsedMembers(BridgedNominalTypeDecl decl,
                                             BridgedArrayRef members);

//===----------------------------------------------------------------------===//
// MARK: VarDecl
//===----------------------------------------------------------------------===//

SWIFT_NAME("BridgedVarDecl.getUserFacingName(self:)")
BRIDGED_INLINE
BridgedStringRef BridgedVarDecl_getUserFacingName(BridgedVarDecl decl);

//===----------------------------------------------------------------------===//
// MARK: Exprs
//===----------------------------------------------------------------------===//

SWIFT_NAME("BridgedCallExpr.createParsed(_:fn:args:)")
BridgedCallExpr BridgedCallExpr_createParsed(BridgedASTContext cContext,
                                             BridgedExpr fn,
                                             BridgedTupleExpr args);

SWIFT_NAME("BridgedClosureExpr.createParsed(_:declContext:body:)")
BridgedClosureExpr
BridgedClosureExpr_createParsed(BridgedASTContext cContext,
                                BridgedDeclContext cDeclContext,
                                BridgedBraceStmt body);

SWIFT_NAME("BridgedUnresolvedDeclRefExpr.createParsed(_:base:loc:)")
BridgedUnresolvedDeclRefExpr BridgedUnresolvedDeclRefExpr_createParsed(
    BridgedASTContext cContext, BridgedIdentifier base, BridgedSourceLoc cLoc);

SWIFT_NAME("BridgedSingleValueStmtExpr.createWithWrappedBranches(_:stmt:"
           "declContext:mustBeExpr:)")
BridgedSingleValueStmtExpr BridgedSingleValueStmtExpr_createWithWrappedBranches(
    BridgedASTContext cContext, BridgedStmt S, BridgedDeclContext cDeclContext,
    bool mustBeExpr);

SWIFT_NAME("BridgedStringLiteralExpr.createParsed(_:value:loc:)")
BridgedStringLiteralExpr
BridgedStringLiteralExpr_createParsed(BridgedASTContext cContext,
                                      BridgedStringRef cStr,
                                      BridgedSourceLoc cTokenLoc);

SWIFT_NAME("BridgedSequenceExpr.createParsed(_:exprs:)")
BridgedSequenceExpr BridgedSequenceExpr_createParsed(BridgedASTContext cContext,
                                                     BridgedArrayRef exprs);

SWIFT_NAME("BridgedTupleExpr.createParsed(_:leftParenLoc:exprs:labels:"
           "labelLocs:rightParenLoc:)")
BridgedTupleExpr BridgedTupleExpr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLParen, BridgedArrayRef subs,
    BridgedArrayRef names, BridgedArrayRef cNameLocs, BridgedSourceLoc cRParen);

SWIFT_NAME("BridgedIntegerLiteralExpr.createParsed(_:value:loc:)")
BridgedIntegerLiteralExpr
BridgedIntegerLiteralExpr_createParsed(BridgedASTContext cContext,
                                       BridgedStringRef cStr,
                                       BridgedSourceLoc cTokenLoc);

SWIFT_NAME("BridgedBooleanLiteralExpr.createParsed(_:value:loc:)")
BridgedBooleanLiteralExpr
BridgedBooleanLiteralExpr_createParsed(BridgedASTContext cContext, bool value,
                                       BridgedSourceLoc cTokenLoc);

SWIFT_NAME("BridgedNilLiteralExpr.createParsed(_:nilKeywordLoc:)")
BridgedNilLiteralExpr
BridgedNilLiteralExpr_createParsed(BridgedASTContext cContext,
                                   BridgedSourceLoc cNilKeywordLoc);

SWIFT_NAME("BridgedArrayExpr.createParsed(_:lSquareLoc:elements:commaLocs:"
           "rSquareLoc:)")
BridgedArrayExpr BridgedArrayExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cLLoc,
                                               BridgedArrayRef elements,
                                               BridgedArrayRef commas,
                                               BridgedSourceLoc cRLoc);

SWIFT_NAME("BridgedUnresolvedDotExpr.createParsed(_:base:dotLoc:name:nameLoc:)")
BridgedUnresolvedDotExpr BridgedUnresolvedDotExpr_createParsed(
    BridgedASTContext cContext, BridgedExpr base, BridgedSourceLoc cDotLoc,
    BridgedIdentifier name, BridgedSourceLoc cNameLoc);

SWIFT_NAME("BridgedExpr.dump(self:)")
void BridgedExpr_dump(BridgedExpr expr);

//===----------------------------------------------------------------------===//
// MARK: Stmts
//===----------------------------------------------------------------------===//

SWIFT_NAME("BridgedBraceStmt.createParsed(_:lBraceLoc:elements:rBraceLoc:)")
BridgedBraceStmt BridgedBraceStmt_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cLBLoc,
                                               BridgedArrayRef elements,
                                               BridgedSourceLoc cRBLoc);

SWIFT_NAME("BridgedIfStmt.createParsed(_:ifKeywordLoc:condition:thenStmt:"
           "elseLoc:elseStmt:)")
BridgedIfStmt BridgedIfStmt_createParsed(BridgedASTContext cContext,
                                         BridgedSourceLoc cIfLoc,
                                         BridgedExpr cond, BridgedStmt then,
                                         BridgedSourceLoc cElseLoc,
                                         BridgedNullableStmt elseStmt);

SWIFT_NAME("BridgedReturnStmt.createParsed(_:returnKeywordLoc:expr:)")
BridgedReturnStmt BridgedReturnStmt_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cLoc,
                                                 BridgedNullableExpr expr);

SWIFT_NAME("BridgedStmt.dump(self:)")
void BridgedStmt_dump(BridgedStmt statement);

//===----------------------------------------------------------------------===//
// MARK: TypeAttributes
//===----------------------------------------------------------------------===//

// Bridged type attribute kinds, which mirror TypeAttrKind exactly.
enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedTypeAttrKind : size_t {
  BridgedTypeAttrKind_autoclosure,
  BridgedTypeAttrKind_convention,
  BridgedTypeAttrKind_noescape,
  BridgedTypeAttrKind_escaping,
  BridgedTypeAttrKind_differentiable,
  BridgedTypeAttrKind_noDerivative,
  BridgedTypeAttrKind_async,
  BridgedTypeAttrKind_Sendable,
  BridgedTypeAttrKind_retroactive,
  BridgedTypeAttrKind_unchecked,
  BridgedTypeAttrKind__local,
  BridgedTypeAttrKind__noMetadata,
  BridgedTypeAttrKind__opaqueReturnTypeOf,
  BridgedTypeAttrKind_block_storage,
  BridgedTypeAttrKind_box,
  BridgedTypeAttrKind_dynamic_self,
  BridgedTypeAttrKind_sil_weak,
  BridgedTypeAttrKind_sil_unowned,
  BridgedTypeAttrKind_sil_unmanaged,
  BridgedTypeAttrKind_error,
  BridgedTypeAttrKind_error_indirect,
  BridgedTypeAttrKind_error_unowned,
  BridgedTypeAttrKind_out,
  BridgedTypeAttrKind_direct,
  BridgedTypeAttrKind_in,
  BridgedTypeAttrKind_inout,
  BridgedTypeAttrKind_inout_aliasable,
  BridgedTypeAttrKind_in_guaranteed,
  BridgedTypeAttrKind_in_constant,
  BridgedTypeAttrKind_pack_owned,
  BridgedTypeAttrKind_pack_guaranteed,
  BridgedTypeAttrKind_pack_inout,
  BridgedTypeAttrKind_pack_out,
  BridgedTypeAttrKind_owned,
  BridgedTypeAttrKind_unowned_inner_pointer,
  BridgedTypeAttrKind_guaranteed,
  BridgedTypeAttrKind_autoreleased,
  BridgedTypeAttrKind_callee_owned,
  BridgedTypeAttrKind_callee_guaranteed,
  BridgedTypeAttrKind_objc_metatype,
  BridgedTypeAttrKind_opened,
  BridgedTypeAttrKind_pack_element,
  BridgedTypeAttrKind_pseudogeneric,
  BridgedTypeAttrKind_unimplementable,
  BridgedTypeAttrKind_yields,
  BridgedTypeAttrKind_yield_once,
  BridgedTypeAttrKind_yield_many,
  BridgedTypeAttrKind_captures_generics,
  BridgedTypeAttrKind_moveOnly,
  BridgedTypeAttrKind_thin,
  BridgedTypeAttrKind_thick,
  BridgedTypeAttrKind_Count
};

SWIFT_NAME("BridgedTypeAttrKind.init(from:)")
BridgedTypeAttrKind BridgedTypeAttrKind_fromString(BridgedStringRef cStr);

SWIFT_NAME("BridgedTypeAttributes.init()")
BridgedTypeAttributes BridgedTypeAttributes_create(void);

SWIFT_NAME("BridgedTypeAttributes.addSimpleAttr(self:kind:atLoc:attrLoc:)")
void BridgedTypeAttributes_addSimpleAttr(BridgedTypeAttributes cAttributes,
                                         BridgedTypeAttrKind kind,
                                         BridgedSourceLoc cAtLoc,
                                         BridgedSourceLoc cAttrLoc);

//===----------------------------------------------------------------------===//
// MARK: TypeReprs
//===----------------------------------------------------------------------===//

/// Bridged parameter specifiers
enum ENUM_EXTENSIBILITY_ATTR(open) BridgedAttributedTypeSpecifier : size_t {
  BridgedAttributedTypeSpecifierInOut,
  BridgedAttributedTypeSpecifierBorrowing,
  BridgedAttributedTypeSpecifierConsuming,
  BridgedAttributedTypeSpecifierLegacyShared,
  BridgedAttributedTypeSpecifierLegacyOwned,
  BridgedAttributedTypeSpecifierConst,
  BridgedAttributedTypeSpecifierIsolated,
};

SWIFT_NAME("BridgedSimpleIdentTypeRepr.createParsed(_:loc:name:)")
BridgedTypeRepr BridgedSimpleIdentTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLoc, BridgedIdentifier id);

SWIFT_NAME(
    "BridgedSpecifierTypeRepr.createParsed(_:base:specifier:specifierLoc:)")
BridgedTypeRepr BridgedSpecifierTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base,
    BridgedAttributedTypeSpecifier specifier, BridgedSourceLoc cSpecifierLoc);

SWIFT_NAME(
    "BridgedArrayTypeRepr.createParsed(_:base:leftSquareLoc:rightSquareLoc:)")
BridgedTypeRepr BridgedArrayTypeRepr_createParsed(BridgedASTContext cContext,
                                                  BridgedTypeRepr base,
                                                  BridgedSourceLoc cLSquareLoc,
                                                  BridgedSourceLoc cRSquareLoc);

SWIFT_NAME(
    "BridgedAttributedTypeRepr.createParsed(_:base:consumingAttributes:)")
BridgedTypeRepr
BridgedAttributedTypeRepr_createParsed(BridgedASTContext cContext,
                                       BridgedTypeRepr base,
                                       BridgedTypeAttributes cAttributes);

SWIFT_NAME("BridgedCompositionTypeRepr.createEmpty(_:anyKeywordLoc:)")
BridgedTypeRepr
BridgedCompositionTypeRepr_createEmpty(BridgedASTContext cContext,
                                       BridgedSourceLoc cAnyLoc);

SWIFT_NAME("BridgedCompositionTypeRepr.createParsed(_:types:ampersandLoc:)")
BridgedTypeRepr
BridgedCompositionTypeRepr_createParsed(BridgedASTContext cContext,
                                        BridgedArrayRef types,
                                        BridgedSourceLoc cFirstAmpLoc);

SWIFT_NAME("BridgedDictionaryTypeRepr.createParsed(_:leftSquareLoc:keyType:"
           "colonLoc:valueType:rightSquareLoc:)")
BridgedTypeRepr BridgedDictionaryTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLSquareLoc,
    BridgedTypeRepr keyType, BridgedSourceLoc cColonloc,
    BridgedTypeRepr valueType, BridgedSourceLoc cRSquareLoc);

SWIFT_NAME("BridgedFunctionTypeRepr.createParsed(_:argsType:asyncLoc:throwsLoc:"
           "thrownType:arrowLoc:resultType:)")
BridgedTypeRepr BridgedFunctionTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr argsTy,
    BridgedSourceLoc cAsyncLoc, BridgedSourceLoc cThrowsLoc,
    BridgedNullableTypeRepr thrownType, BridgedSourceLoc cArrowLoc,
    BridgedTypeRepr resultType);

SWIFT_NAME("BridgedGenericIdentTypeRepr.createParsed(_:name:nameLoc:"
           "genericArgs:leftAngleLoc:rightAngleLoc:)")
BridgedTypeRepr BridgedGenericIdentTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedIdentifier name,
    BridgedSourceLoc cNameLoc, BridgedArrayRef genericArgs,
    BridgedSourceLoc cLAngleLoc, BridgedSourceLoc cRAngleLoc);

SWIFT_NAME("BridgedOptionalTypeRepr.createParsed(_:base:questionLoc:)")
BridgedTypeRepr
BridgedOptionalTypeRepr_createParsed(BridgedASTContext cContext,
                                     BridgedTypeRepr base,
                                     BridgedSourceLoc cQuestionLoc);

SWIFT_NAME("BridgedImplicitlyUnwrappedOptionalTypeRepr.createParsed(_:base:"
           "exclaimLoc:)")
BridgedTypeRepr BridgedImplicitlyUnwrappedOptionalTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base,
    BridgedSourceLoc cExclamationLoc);

SWIFT_NAME("BridgedMemberTypeRepr.createParsed(_:base:members:)")
BridgedTypeRepr
BridgedMemberTypeRepr_createParsed(BridgedASTContext cContext,
                                   BridgedTypeRepr baseComponent,
                                   BridgedArrayRef bridgedMemberComponents);

SWIFT_NAME("BridgedMetatypeTypeRepr.createParsed(_:base:typeKeywordLoc:)")
BridgedTypeRepr BridgedMetatypeTypeRepr_createParsed(BridgedASTContext cContext,
                                                     BridgedTypeRepr baseType,
                                                     BridgedSourceLoc cTypeLoc);

SWIFT_NAME("BridgedProtocolTypeRepr.createParsed(_:base:protocolKeywordLoc:)")
BridgedTypeRepr
BridgedProtocolTypeRepr_createParsed(BridgedASTContext cContext,
                                     BridgedTypeRepr baseType,
                                     BridgedSourceLoc cProtoLoc);

SWIFT_NAME(
    "BridgedPackExpansionTypeRepr.createParsed(_:base:repeatKeywordLoc:)")
BridgedTypeRepr
BridgedPackExpansionTypeRepr_createParsed(BridgedASTContext cContext,
                                          BridgedTypeRepr base,
                                          BridgedSourceLoc cRepeatLoc);

SWIFT_NAME(
    "BridgedTupleTypeRepr.createParsed(_:elements:leftParenLoc:rightParenLoc:)")
BridgedTypeRepr BridgedTupleTypeRepr_createParsed(BridgedASTContext cContext,
                                                  BridgedArrayRef elements,
                                                  BridgedSourceLoc cLParenLoc,
                                                  BridgedSourceLoc cRParenLoc);

SWIFT_NAME("BridgedNamedOpaqueReturnTypeRepr.createParsed(_:base:)")
BridgedTypeRepr
BridgedNamedOpaqueReturnTypeRepr_createParsed(BridgedASTContext cContext,
                                              BridgedTypeRepr baseTy);

SWIFT_NAME("BridgedOpaqueReturnTypeRepr.createParsed(_:someKeywordLoc:base:)")
BridgedTypeRepr
BridgedOpaqueReturnTypeRepr_createParsed(BridgedASTContext cContext,
                                         BridgedSourceLoc cOpaqueLoc,
                                         BridgedTypeRepr baseTy);

SWIFT_NAME("BridgedExistentialTypeRepr.createParsed(_:anyKeywordLoc:base:)")
BridgedTypeRepr
BridgedExistentialTypeRepr_createParsed(BridgedASTContext cContext,
                                        BridgedSourceLoc cAnyLoc,
                                        BridgedTypeRepr baseTy);

SWIFT_NAME("BridgedVarargTypeRepr.createParsed(_:base:ellipsisLoc:)")
BridgedTypeRepr
BridgedVarargTypeRepr_createParsed(BridgedASTContext cContext,
                                   BridgedTypeRepr base,
                                   BridgedSourceLoc cEllipsisLoc);

SWIFT_NAME("BridgedTypeRepr.dump(self:)")
void BridgedTypeRepr_dump(BridgedTypeRepr type);

//===----------------------------------------------------------------------===//
// MARK: Misc
//===----------------------------------------------------------------------===//

struct BridgedTupleTypeElement {
  BridgedIdentifier Name;
  BridgedSourceLoc NameLoc;
  BridgedIdentifier SecondName;
  BridgedSourceLoc SecondNameLoc;
  BridgedSourceLoc UnderscoreLoc;
  BridgedSourceLoc ColonLoc;
  BridgedTypeRepr Type;
  BridgedSourceLoc TrailingCommaLoc;
};

enum ENUM_EXTENSIBILITY_ATTR(open) BridgedRequirementReprKind : size_t {
  /// A type bound T : P, where T is a type that depends on a generic
  /// parameter and P is some type that should bound T, either as a concrete
  /// supertype or a protocol to which T must conform.
  BridgedRequirementReprKindTypeConstraint,

  /// A same-type requirement T == U, where T and U are types that shall be
  /// equivalent.
  BridgedRequirementReprKindSameType,

  /// A layout bound T : L, where T is a type that depends on a generic
  /// parameter and L is some layout specification that should bound T.
  BridgedRequirementReprKindLayoutConstraint,

  // Note: there is code that packs this enum in a 2-bit bitfield.  Audit users
  // when adding enumerators.
};

struct BridgedRequirementRepr {
  BridgedSourceLoc SeparatorLoc;
  BridgedRequirementReprKind Kind;
  BridgedTypeRepr FirstType;
  BridgedTypeRepr SecondType;
  // FIXME: Handle Layout Requirements
};

enum ENUM_EXTENSIBILITY_ATTR(open) BridgedMacroDefinitionKind : size_t {
  /// An expanded macro.
  BridgedExpandedMacro = 0,
  /// An external macro, spelled with either the old spelling (Module.Type)
  /// or the new spelling `#externalMacro(module: "Module", type: "Type")`.
  BridgedExternalMacro,
  /// The builtin definition for "externalMacro".
  BridgedBuiltinExternalMacro
};

SWIFT_NAME("BridgedGenericParamList.createParsed(_:leftAngleLoc:parameters:"
           "genericWhereClause:rightAngleLoc:)")
BridgedGenericParamList BridgedGenericParamList_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLeftAngleLoc,
    BridgedArrayRef cParameters,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceLoc cRightAngleLoc);

SWIFT_NAME(
    "BridgedGenericTypeParamDecl.createParsed(_:declContext:eachKeywordLoc:"
    "name:nameLoc:inheritedType:index:)")
BridgedGenericTypeParamDecl BridgedGenericTypeParamDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cEachLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableTypeRepr opaqueInheritedType,
    size_t index);

SWIFT_NAME(
    "BridgedTrailingWhereClause.createParsed(_:whereKeywordLoc:requirements:)")
BridgedTrailingWhereClause
BridgedTrailingWhereClause_createParsed(BridgedASTContext cContext,
                                        BridgedSourceLoc cWhereKeywordLoc,
                                        BridgedArrayRef cRequirements);

SWIFT_NAME("BridgedParameterList.createParsed(_:leftParenLoc:parameters:"
           "rightParenLoc:)")
BridgedParameterList BridgedParameterList_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLeftParenLoc,
    BridgedArrayRef cParameters, BridgedSourceLoc cRightParenLoc);

//===----------------------------------------------------------------------===//
// MARK: Plugins
//===----------------------------------------------------------------------===//

SWIFT_BEGIN_ASSUME_NONNULL

typedef void *PluginHandle;
typedef const void *PluginCapabilityPtr;

/// Set a capability data to the plugin object. Since the data is just a opaque
/// pointer, it's not used in AST at all.
void Plugin_setCapability(PluginHandle handle,
                          PluginCapabilityPtr _Nullable data);

/// Get a capability data set by \c Plugin_setCapability .
PluginCapabilityPtr _Nullable Plugin_getCapability(PluginHandle handle);

/// Get the executable file path of the plugin.
const char *Plugin_getExecutableFilePath(PluginHandle handle);

/// Lock the plugin. Clients should lock it during sending and recving the
/// response.
void Plugin_lock(PluginHandle handle);

/// Unlock the plugin.
void Plugin_unlock(PluginHandle handle);

/// Launch the plugin if it's not running.
bool Plugin_spawnIfNeeded(PluginHandle handle);

/// Sends the message to the plugin, returns true if there was an error.
/// Clients should receive the response  by \c Plugin_waitForNextMessage .
bool Plugin_sendMessage(PluginHandle handle, const BridgedData data);

/// Receive a message from the plugin.
bool Plugin_waitForNextMessage(PluginHandle handle, BridgedData *data);

SWIFT_END_ASSUME_NONNULL

SWIFT_END_NULLABILITY_ANNOTATIONS

#ifndef PURE_BRIDGING_MODE
// In _not_ PURE_BRIDGING_MODE, bridging functions are inlined and therefore
// included in the header file. This is because they rely on C++ headers that
// we don't want to pull in when using "pure bridging mode".
#include "ASTBridgingImpl.h"
#endif

#endif // SWIFT_AST_ASTBRIDGING_H
