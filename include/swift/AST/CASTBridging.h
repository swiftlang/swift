//===--- CASTBridging.h - C header for the AST bridging layer ----*- C -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_C_AST_ASTBRIDGING_H
#define SWIFT_C_AST_ASTBRIDGING_H

#include "swift/Basic/CBasicBridging.h"
#include "swift/Basic/Compiler.h"
#include "swift/Basic/Nullability.h"

#include <stddef.h>

// NOTE: DO NOT #include any stdlib headers here. e.g. <stdint.h>. Those are
// part of "Darwin"/"Glibc" module, so when a Swift file imports this header,
// it causes importing the "Darwin"/"Glibc" overlay module. That violates
// layering. i.e. Darwin overlay is created by Swift compiler.

#if __has_attribute(swift_name)
#define SWIFT_NAME(NAME) __attribute__((swift_name(NAME)))
#else
#define SWIFT_NAME(NAME)
#endif

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

// Define the bridging wrappers for each AST node.
#define AST_BRIDGING_WRAPPER_NONNULL(Name)                                     \
  typedef struct {                                                             \
    void *_Nonnull raw;                                                        \
  } Bridged##Name;

// For nullable nodes, define both a nullable and non-null variant.
#define AST_BRIDGING_WRAPPER_NULLABLE(Name)                                    \
  typedef struct {                                                             \
    void *_Nullable raw;                                                       \
  } BridgedNullable##Name;                                                     \
                                                                               \
  typedef struct {                                                             \
    void *_Nonnull raw;                                                        \
  } Bridged##Name;

#include "swift/AST/ASTBridgingWrappers.def"

#ifdef __cplusplus
extern "C" {
#endif

// Declare `.asDecl` on each BridgedXXXDecl type, which upcasts a wrapper for
// a Decl subclass to a BridgedDecl.
#define DECL(Id, Parent)                                                       \
  SWIFT_NAME("getter:Bridged" #Id "Decl.asDecl(self:)")                        \
  BridgedDecl Id##Decl_asDecl(Bridged##Id##Decl decl);
#define ABSTRACT_DECL(Id, Parent) DECL(Id, Parent)
#include "swift/AST/DeclNodes.def"

// Declare `.asDeclContext` on each BridgedXXXDecl type that's also a
// DeclContext.
#define DECL(Id, Parent)
#define CONTEXT_DECL(Id, Parent)                                               \
  SWIFT_NAME("getter:Bridged" #Id "Decl.asDeclContext(self:)")                 \
  BridgedDeclContext Id##Decl_asDeclContext(Bridged##Id##Decl decl);
#define ABSTRACT_CONTEXT_DECL(Id, Parent) CONTEXT_DECL(Id, Parent)
#include "swift/AST/DeclNodes.def"

// Declare `.asStmt` on each BridgedXXXStmt type, which upcasts a wrapper for
// a Stmt subclass to a BridgedStmt.
#define STMT(Id, Parent)                                                       \
  SWIFT_NAME("getter:Bridged" #Id "Stmt.asStmt(self:)")                        \
  BridgedStmt Id##Stmt_asStmt(Bridged##Id##Stmt stmt);
#define ABSTRACT_STMT(Id, Parent) STMT(Id, Parent)
#include "swift/AST/StmtNodes.def"

// Declare `.asExpr` on each BridgedXXXExpr type, which upcasts a wrapper for
// a Expr subclass to a BridgedExpr.
#define EXPR(Id, Parent)                                                       \
  SWIFT_NAME("getter:Bridged" #Id "Expr.asExpr(self:)")                        \
  BridgedExpr Id##Expr_asExpr(Bridged##Id##Expr expr);
#define ABSTRACT_EXPR(Id, Parent) EXPR(Id, Parent)
#include "swift/AST/ExprNodes.def"

// Declare `.asTypeRepr` on each BridgedXXXTypeRepr type, which upcasts a
// wrapper for a TypeRepr subclass to a BridgedTypeRepr.
#define TYPEREPR(Id, Parent)                                                   \
  SWIFT_NAME("getter:Bridged" #Id "TypeRepr.asTypeRepr(self:)")                \
  BridgedTypeRepr Id##TypeRepr_asTypeRepr(Bridged##Id##TypeRepr typeRepr);
#define ABSTRACT_TYPEREPR(Id, Parent) TYPEREPR(Id, Parent)
#include "swift/AST/TypeReprNodes.def"

#ifdef __cplusplus
}
#endif

SWIFT_BEGIN_ASSUME_NONNULL

typedef struct {
  const unsigned char *_Nullable data;
  size_t length;
} BridgedString;

typedef struct {
  const void *_Nullable data;
  size_t numElements;
} BridgedArrayRef;

typedef struct BridgedASTContext {
  void *raw;
} BridgedASTContext;

typedef struct BridgedSourceLoc {
  const void *_Nullable raw;
} BridgedSourceLoc;

typedef struct {
  BridgedSourceLoc startLoc;
  BridgedSourceLoc endLoc;
} BridgedSourceRange;

typedef struct BridgedIdentifier {
  const void *_Nullable raw;
} BridgedIdentifier;

typedef struct {
  void *start;
  size_t byteLength;
} BridgedCharSourceRange;

typedef struct {
  BridgedIdentifier Name;
  BridgedSourceLoc NameLoc;
  BridgedIdentifier SecondName;
  BridgedSourceLoc SecondNameLoc;
  BridgedSourceLoc UnderscoreLoc;
  BridgedSourceLoc ColonLoc;
  BridgedTypeRepr Type;
  BridgedSourceLoc TrailingCommaLoc;
} BridgedTupleTypeElement;

typedef enum ENUM_EXTENSIBILITY_ATTR(open) BridgedRequirementReprKind : size_t {
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
} BridgedRequirementReprKind;

typedef struct {
  BridgedSourceLoc SeparatorLoc;
  BridgedRequirementReprKind Kind;
  BridgedTypeRepr FirstType;
  BridgedTypeRepr SecondType;
  // FIXME: Handle Layout Requirements
} BridgedRequirementRepr;

/// Diagnostic severity when reporting diagnostics.
typedef enum ENUM_EXTENSIBILITY_ATTR(open) BridgedDiagnosticSeverity : size_t {
  BridgedFatalError,
  BridgedError,
  BridgedWarning,
  BridgedRemark,
  BridgedNote,
} BridgedDiagnosticSeverity;

typedef struct BridgedDiagnostic {
  void *raw;
} BridgedDiagnostic;

typedef struct BridgedDiagnosticEngine {
  void *raw;
} BridgedDiagnosticEngine;

typedef enum ENUM_EXTENSIBILITY_ATTR(open) BridgedMacroDefinitionKind : size_t {
  /// An expanded macro.
  BridgedExpandedMacro = 0,
  /// An external macro, spelled with either the old spelling (Module.Type)
  /// or the new spelling `#externalMacro(module: "Module", type: "Type")`.
  BridgedExternalMacro,
  /// The builtin definition for "externalMacro".
  BridgedBuiltinExternalMacro
} BridgedMacroDefinitionKind;

/// Bridged parameter specifiers
typedef enum ENUM_EXTENSIBILITY_ATTR(open) BridgedAttributedTypeSpecifier : size_t {
  BridgedAttributedTypeSpecifierInOut,
  BridgedAttributedTypeSpecifierBorrowing,
  BridgedAttributedTypeSpecifierConsuming,
  BridgedAttributedTypeSpecifierLegacyShared,
  BridgedAttributedTypeSpecifierLegacyOwned,
  BridgedAttributedTypeSpecifierConst,
  BridgedAttributedTypeSpecifierIsolated,
} BridgedAttributedTypeSpecifier;

// Bridged type attribute kinds, which mirror TypeAttrKind exactly.
typedef enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedTypeAttrKind : size_t {
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
} BridgedTypeAttrKind;

typedef enum ENUM_EXTENSIBILITY_ATTR(open) ASTNodeKind : size_t {
  ASTNodeKindExpr,
  ASTNodeKindStmt,
  ASTNodeKindDecl
} ASTNodeKind;

typedef struct BridgedASTNode {
  void *ptr;
  ASTNodeKind kind;
} BridgedASTNode;

typedef struct BridgedTypeAttributes {
  void *raw;
} BridgedTypeAttributes;

struct BridgedIdentifierAndSourceLoc {
  BridgedIdentifier name;
  BridgedSourceLoc nameLoc;
};

#ifdef __cplusplus
extern "C" {

#define _Bool bool
#endif

// Diagnostics

/// Create a new diagnostic with the given severity, location, and diagnostic
/// text.
///
/// \returns a diagnostic instance that can be extended with additional
/// information and then must be finished via \c SwiftDiagnostic_finish.
SWIFT_NAME("BridgedDiagnostic.init(at:message:severity:engine:)")
BridgedDiagnostic Diagnostic_create(BridgedSourceLoc cLoc, BridgedString cText,
                                    BridgedDiagnosticSeverity severity,
                                    BridgedDiagnosticEngine cDiags);

/// Highlight a source range as part of the diagnostic.
SWIFT_NAME("BridgedDiagnostic.highlight(self:start:end:)")
void Diagnostic_highlight(BridgedDiagnostic cDiag, BridgedSourceLoc cStartLoc,
                          BridgedSourceLoc cEndLoc);

/// Add a Fix-It to replace a source range as part of the diagnostic.
SWIFT_NAME("BridgedDiagnostic.fixItReplace(self:start:end:replacement:)")
void Diagnostic_fixItReplace(BridgedDiagnostic cDiag,
                             BridgedSourceLoc cStartLoc,
                             BridgedSourceLoc cEndLoc,
                             BridgedString cReplaceText);

/// Finish the given diagnostic and emit it.
SWIFT_NAME("BridgedDiagnostic.finish(self:)")
void Diagnostic_finish(BridgedDiagnostic cDiag);

SWIFT_NAME("BridgedASTContext.getIdentifier(self:_:)")
BridgedIdentifier ASTContext_getIdentifier(BridgedASTContext cContext,
                                           BridgedString cStr);

SWIFT_NAME("BridgedASTContext.langOptsHasFeature(self:_:)")
_Bool ASTContext_langOptsHasFeature(BridgedASTContext cContext,
                                    BridgedFeature feature);

SWIFT_NAME(
    "BridgedTopLevelCodeDecl.createParsed(_:declContext:startLoc:stmt:endLoc:)")
BridgedTopLevelCodeDecl
TopLevelCodeDecl_createStmt(BridgedASTContext cContext,
                            BridgedDeclContext cDeclContext,
                            BridgedSourceLoc cStartLoc, BridgedStmt statement,
                            BridgedSourceLoc cEndLoc);

SWIFT_NAME(
    "BridgedTopLevelCodeDecl.createParsed(_:declContext:startLoc:expr:endLoc:)")
BridgedTopLevelCodeDecl
TopLevelCodeDecl_createExpr(BridgedASTContext cContext,
                            BridgedDeclContext cDeclContext,
                            BridgedSourceLoc cStartLoc, BridgedExpr expression,
                            BridgedSourceLoc cEndLoc);

SWIFT_NAME("BridgedReturnStmt.createParsed(_:returnKeywordLoc:expr:)")
BridgedReturnStmt ReturnStmt_createParsed(BridgedASTContext cContext,
                                          BridgedSourceLoc cLoc,
                                          BridgedNullableExpr expr);

SWIFT_NAME("BridgedSequenceExpr.createParsed(_:exprs:)")
BridgedSequenceExpr SequenceExpr_createParsed(BridgedASTContext cContext,
                                              BridgedArrayRef exprs);

SWIFT_NAME("BridgedSourceLoc.advanced(self:by:)")
BridgedSourceLoc SourceLoc_advanced(BridgedSourceLoc cLoc, size_t len);

SWIFT_NAME("BridgedTupleExpr.createParsed(_:leftParenLoc:exprs:labels:"
           "labelLocs:rightParenLoc:)")
BridgedTupleExpr
TupleExpr_createParsed(BridgedASTContext cContext, BridgedSourceLoc cLParen,
                       BridgedArrayRef subs, BridgedArrayRef names,
                       BridgedArrayRef cNameLocs, BridgedSourceLoc cRParen);

SWIFT_NAME("BridgedCallExpr.createParsed(_:fn:args:)")
BridgedCallExpr CallExpr_createParsed(BridgedASTContext cContext,
                                      BridgedExpr fn, BridgedTupleExpr args);

SWIFT_NAME("BridgedUnresolvedDeclRefExpr.createParsed(_:base:loc:)")
BridgedUnresolvedDeclRefExpr UnresolvedDeclRefExpr_createParsed(
    BridgedASTContext cContext, BridgedIdentifier base, BridgedSourceLoc cLoc);

SWIFT_NAME("BridgedStringLiteralExpr.createParsed(_:value:loc:)")
BridgedStringLiteralExpr
StringLiteralExpr_createParsed(BridgedASTContext cContext, BridgedString cStr,
                               BridgedSourceLoc cTokenLoc);

SWIFT_NAME("BridgedIntegerLiteralExpr.createParsed(_:value:loc:)")
BridgedIntegerLiteralExpr
IntegerLiteralExpr_createParsed(BridgedASTContext cContext, BridgedString cStr,
                                BridgedSourceLoc cTokenLoc);

SWIFT_NAME("BridgedBooleanLiteralExpr.createParsed(_:value:loc:)")
BridgedBooleanLiteralExpr
BooleanLiteralExpr_createParsed(BridgedASTContext cContext, _Bool value,
                                BridgedSourceLoc cTokenLoc);

SWIFT_NAME("BridgedNilLiteralExpr.createParsed(_:nilKeywordLoc:)")
BridgedNilLiteralExpr
NilLiteralExpr_createParsed(BridgedASTContext cContext,
                            BridgedSourceLoc cNilKeywordLoc);

SWIFT_NAME("BridgedArrayExpr.createParsed(_:lSquareLoc:elements:commaLocs:"
           "rSquareLoc:)")
BridgedArrayExpr ArrayExpr_createParsed(BridgedASTContext cContext,
                                        BridgedSourceLoc cLLoc,
                                        BridgedArrayRef elements,
                                        BridgedArrayRef commas,
                                        BridgedSourceLoc cRLoc);

SWIFT_NAME(
    "BridgedPatternBindingDecl.createParsed(_:declContext:bindingKeywordLoc:"
    "nameExpr:initializer:isStatic:isLet:)")
BridgedPatternBindingDecl PatternBindingDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cBindingKeywordLoc, BridgedExpr opaqueNameExpr,
    BridgedExpr opaqueInitExpr, _Bool isStatic, _Bool isLet);

SWIFT_NAME("BridgedSingleValueStmtExpr.createWithWrappedBranches(_:stmt:"
           "declContext:mustBeExpr:)")
BridgedSingleValueStmtExpr SingleValueStmtExpr_createWithWrappedBranches(
    BridgedASTContext cContext, BridgedStmt S, BridgedDeclContext cDeclContext,
    _Bool mustBeExpr);

SWIFT_NAME("BridgedIfStmt.createParsed(_:ifKeywordLoc:condition:thenStmt:"
           "elseLoc:elseStmt:)")
BridgedIfStmt IfStmt_createParsed(BridgedASTContext cContext,
                                  BridgedSourceLoc cIfLoc, BridgedExpr cond,
                                  BridgedStmt then, BridgedSourceLoc cElseLoc,
                                  BridgedNullableStmt elseStmt);

SWIFT_NAME("BridgedBraceStmt.createParsed(_:lBraceLoc:elements:rBraceLoc:)")
BridgedBraceStmt BraceStmt_createParsed(BridgedASTContext cContext,
                                        BridgedSourceLoc cLBLoc,
                                        BridgedArrayRef elements,
                                        BridgedSourceLoc cRBLoc);

SWIFT_NAME("BridgedParamDecl.createParsed(_:declContext:specifierLoc:firstName:"
           "firstNameLoc:secondName:secondNameLoc:type:defaultValue:)")
BridgedParamDecl ParamDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cSpecifierLoc, BridgedIdentifier cFirstName,
    BridgedSourceLoc cFirstNameLoc, BridgedIdentifier cSecondName,
    BridgedSourceLoc cSecondNameLoc, BridgedNullableTypeRepr type,
    BridgedNullableExpr defaultValue);

SWIFT_NAME("BridgedConstructorDecl.setParsedBody(self:_:)")
void ConstructorDecl_setParsedBody(BridgedConstructorDecl decl,
                                   BridgedBraceStmt body);

SWIFT_NAME("BridgedFuncDecl.setParsedBody(self:_:)")
void FuncDecl_setParsedBody(BridgedFuncDecl decl, BridgedBraceStmt body);

SWIFT_NAME("BridgedDestructorDecl.setParsedBody(self:_:)")
void DestructorDecl_setParsedBody(BridgedDestructorDecl decl,
                                  BridgedBraceStmt body);

SWIFT_NAME(
    "BridgedFuncDecl.createParsed(_:declContext:staticLoc:funcKeywordLoc:"
    "name:nameLoc:genericParamList:parameterList:asyncSpecifierLoc:"
    "throwsSpecifierLoc:thrownType:returnType:genericWhereClause:)")
BridgedFuncDecl FuncDecl_createParsed(
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
BridgedConstructorDecl ConstructorDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cInitKeywordLoc, BridgedSourceLoc cFailabilityMarkLoc,
    _Bool isIUO, BridgedNullableGenericParamList genericParams,
    BridgedParameterList parameterList, BridgedSourceLoc cAsyncLoc,
    BridgedSourceLoc cThrowsLoc, BridgedNullableTypeRepr thrownType,
    BridgedNullableTrailingWhereClause genericWhereClause);

SWIFT_NAME(
    "BridgedDestructorDecl.createParsed(_:declContext:deinitKeywordLoc:)")
BridgedDestructorDecl
DestructorDecl_createParsed(BridgedASTContext cContext,
                            BridgedDeclContext cDeclContext,
                            BridgedSourceLoc cDeinitKeywordLoc);

SWIFT_NAME("BridgedSimpleIdentTypeRepr.createParsed(_:loc:name:)")
BridgedTypeRepr SimpleIdentTypeRepr_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cLoc,
                                                 BridgedIdentifier id);

SWIFT_NAME("BridgedUnresolvedDotExpr.createParsed(_:base:dotLoc:name:nameLoc:)")
BridgedUnresolvedDotExpr
UnresolvedDotExpr_createParsed(BridgedASTContext cContext, BridgedExpr base,
                               BridgedSourceLoc cDotLoc, BridgedIdentifier name,
                               BridgedSourceLoc cNameLoc);

SWIFT_NAME("BridgedClosureExpr.createParsed(_:declContext:body:)")
BridgedClosureExpr ClosureExpr_createParsed(BridgedASTContext cContext,
                                            BridgedDeclContext cDeclContext,
                                            BridgedBraceStmt body);

SWIFT_NAME(
    "BridgedTypeAliasDecl.createParsed(_:declContext:typealiasKeywordLoc:name:"
    "nameLoc:genericParamList:equalLoc:underlyingType:genericWhereClause:)")
BridgedTypeAliasDecl TypeAliasDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cAliasKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedSourceLoc cEqualLoc, BridgedTypeRepr underlyingType,
    BridgedNullableTrailingWhereClause genericWhereClause);

SWIFT_NAME("BridgedNominalTypeDecl.setParsedMembers(self:_:)")
void NominalTypeDecl_setParsedMembers(BridgedNominalTypeDecl decl,
                                      BridgedArrayRef members);

SWIFT_NAME("BridgedExtensionDecl.setParsedMembers(self:_:)")
void ExtensionDecl_setParsedMembers(BridgedExtensionDecl decl,
                                    BridgedArrayRef members);

SWIFT_NAME(
    "BridgedEnumDecl.createParsed(_:declContext:enumKeywordLoc:name:nameLoc:"
    "genericParamList:inheritedTypes:genericWhereClause:braceRange:)")
BridgedNominalTypeDecl EnumDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cEnumKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange);

SWIFT_NAME(
    "BridgedEnumCaseDecl.createParsed(declContext:caseKeywordLoc:elements:)")
BridgedEnumCaseDecl EnumCaseDecl_createParsed(BridgedDeclContext cDeclContext,
                                              BridgedSourceLoc cCaseKeywordLoc,
                                              BridgedArrayRef cElements);

SWIFT_NAME("BridgedEnumElementDecl.createParsed(_:declContext:name:nameLoc:"
           "parameterList:equalsLoc:rawValue:)")
BridgedEnumElementDecl EnumElementDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedIdentifier cName, BridgedSourceLoc cNameLoc,
    BridgedNullableParameterList parameterList, BridgedSourceLoc cEqualsLoc,
    BridgedNullableExpr opaqueRawValue);

SWIFT_NAME("BridgedStructDecl.createParsed(_:declContext:structKeywordLoc:name:"
           "nameLoc:genericParamList:inheritedTypes:genericWhereClause:"
           "braceRange:)")
BridgedNominalTypeDecl StructDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStructKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange);

SWIFT_NAME(
    "BridgedClassDecl.createParsed(_:declContext:classKeywordLoc:name:nameLoc:"
    "genericParamList:inheritedTypes:genericWhereClause:braceRange:isActor:)")
BridgedNominalTypeDecl ClassDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cClassKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange, _Bool isActor);

SWIFT_NAME(
    "BridgedProtocolDecl.createParsed(_:declContext:protocolKeywordLoc:name:"
    "nameLoc:primaryAssociatedTypeNames:inheritedTypes:"
    "genericWhereClause:braceRange:)")
BridgedNominalTypeDecl ProtocolDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cProtocolKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedArrayRef cPrimaryAssociatedTypeNames,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange);

SWIFT_NAME("BridgedAssociatedTypeDecl.createParsed(_:declContext:"
           "associatedtypeKeywordLoc:name:nameLoc:inheritedTypes:defaultType:"
           "genericWhereClause:)")
BridgedAssociatedTypeDecl AssociatedTypeDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cAssociatedtypeKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedArrayRef cInheritedTypes,
    BridgedNullableTypeRepr opaqueDefaultType,
    BridgedNullableTrailingWhereClause genericWhereClause);

SWIFT_NAME(
    "BridgedExtensionDecl.createParsed(_:declContext:extensionKeywordLoc:"
    "extendedType:inheritedTypes:genericWhereClause:braceRange:)")
BridgedExtensionDecl ExtensionDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cExtensionKeywordLoc, BridgedTypeRepr opaqueExtendedType,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange);

typedef enum ENUM_EXTENSIBILITY_ATTR(closed) {
  BridgedOperatorFixityInfix,
  BridgedOperatorFixityPrefix,
  BridgedOperatorFixityPostfix,
} BridgedOperatorFixity;

SWIFT_NAME("BridgedOperatorDecl.createParsed(_:declContext:fixity:"
           "operatorKeywordLoc:name:nameLoc:colonLoc:precedenceGroupName:"
           "precedenceGroupLoc:)")
BridgedOperatorDecl OperatorDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedOperatorFixity cFixity, BridgedSourceLoc cOperatorKeywordLoc,
    BridgedIdentifier cName, BridgedSourceLoc cNameLoc,
    BridgedSourceLoc cColonLoc, BridgedIdentifier cPrecedenceGroupName,
    BridgedSourceLoc cPrecedenceGroupLoc);

typedef enum ENUM_EXTENSIBILITY_ATTR(closed) {
  BridgedAssociativityNone,
  BridgedAssociativityLeft,
  BridgedAssociativityRight,
} BridgedAssociativity;

SWIFT_NAME("BridgedPrecedenceGroupDecl.createParsed(declContext:"
           "precedencegroupKeywordLoc:name:nameLoc:leftBraceLoc:"
           "associativityLabelLoc:associativityValueLoc:associativity:"
           "assignmentLabelLoc:assignmentValueLoc:isAssignment:"
           "higherThanKeywordLoc:higherThanNames:lowerThanKeywordLoc:"
           "lowerThanNames:rightBraceLoc:)")
BridgedPrecedenceGroupDecl PrecedenceGroupDecl_createParsed(
    BridgedDeclContext cDeclContext,
    BridgedSourceLoc cPrecedencegroupKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedSourceLoc cLeftBraceLoc,
    BridgedSourceLoc cAssociativityKeywordLoc,
    BridgedSourceLoc cAssociativityValueLoc,
    BridgedAssociativity cAssociativity, BridgedSourceLoc cAssignmentKeywordLoc,
    BridgedSourceLoc cAssignmentValueLoc, _Bool isAssignment,
    BridgedSourceLoc cHigherThanKeywordLoc, BridgedArrayRef cHigherThanNames,
    BridgedSourceLoc cLowerThanKeywordLoc, BridgedArrayRef cLowerThanNames,
    BridgedSourceLoc cRightBraceLoc);

typedef enum ENUM_EXTENSIBILITY_ATTR(open) {
  BridgedImportKindModule,
  BridgedImportKindType,
  BridgedImportKindStruct,
  BridgedImportKindClass,
  BridgedImportKindEnum,
  BridgedImportKindProtocol,
  BridgedImportKindVar,
  BridgedImportKindFunc,
} BridgedImportKind;

SWIFT_NAME("BridgedImportDecl.createParsed(_:declContext:importKeywordLoc:"
           "importKind:importKindLoc:path:)")
BridgedImportDecl ImportDecl_createParsed(BridgedASTContext cContext,
                                          BridgedDeclContext cDeclContext,
                                          BridgedSourceLoc cImportKeywordLoc,
                                          BridgedImportKind cImportKind,
                                          BridgedSourceLoc cImportKindLoc,
                                          BridgedArrayRef cImportPathElements);

SWIFT_NAME("BridgedGenericParamList.createParsed(_:leftAngleLoc:parameters:"
           "genericWhereClause:rightAngleLoc:)")
BridgedGenericParamList GenericParamList_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLeftAngleLoc,
    BridgedArrayRef cParameters,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceLoc cRightAngleLoc);

SWIFT_NAME(
    "BridgedGenericTypeParamDecl.createParsed(_:declContext:eachKeywordLoc:"
    "name:nameLoc:inheritedType:index:)")
BridgedGenericTypeParamDecl GenericTypeParamDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cEachLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableTypeRepr opaqueInheritedType,
    size_t index);

SWIFT_NAME(
    "BridgedTrailingWhereClause.createParsed(_:whereKeywordLoc:requirements:)")
BridgedTrailingWhereClause
TrailingWhereClause_createParsed(BridgedASTContext cContext,
                                 BridgedSourceLoc cWhereKeywordLoc,
                                 BridgedArrayRef cRequirements);

SWIFT_NAME("BridgedParameterList.createParsed(_:leftParenLoc:parameters:"
           "rightParenLoc:)")
BridgedParameterList ParameterList_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLeftParenLoc,
    BridgedArrayRef cParameters, BridgedSourceLoc cRightParenLoc);

BridgedTypeAttrKind TypeAttrKind_fromString(BridgedString cStr);
BridgedTypeAttributes TypeAttributes_create(void);
void TypeAttributes_addSimpleAttr(BridgedTypeAttributes cAttributes,
                                  BridgedTypeAttrKind kind,
                                  BridgedSourceLoc cAtLoc,
                                  BridgedSourceLoc cAttrLoc);

SWIFT_NAME(
    "BridgedArrayTypeRepr.createParsed(_:base:leftSquareLoc:rightSquareLoc:)")
BridgedTypeRepr ArrayTypeRepr_createParsed(BridgedASTContext cContext,
                                           BridgedTypeRepr base,
                                           BridgedSourceLoc cLSquareLoc,
                                           BridgedSourceLoc cRSquareLoc);

SWIFT_NAME("BridgedAttributedTypeRepr.createParsed(_:base:attributes:)")
BridgedTypeRepr
AttributedTypeRepr_createParsed(BridgedASTContext cContext,
                                BridgedTypeRepr base,
                                BridgedTypeAttributes cAttributes);

SWIFT_NAME(
    "BridgedSpecifierTypeRepr.createParsed(_:base:specifier:specifierLoc:)")
BridgedTypeRepr
SpecifierTypeRepr_createParsed(BridgedASTContext cContext, BridgedTypeRepr base,
                               BridgedAttributedTypeSpecifier specifier,
                               BridgedSourceLoc cSpecifierLoc);

SWIFT_NAME("BridgedCompositionTypeRepr.createEmpty(_:anyKeywordLoc:)")
BridgedTypeRepr CompositionTypeRepr_createEmpty(BridgedASTContext cContext,
                                                BridgedSourceLoc cAnyLoc);

SWIFT_NAME("BridgedCompositionTypeRepr.createParsed(_:types:ampersandLoc:)")
BridgedTypeRepr CompositionTypeRepr_createParsed(BridgedASTContext cContext,
                                                 BridgedArrayRef types,
                                                 BridgedSourceLoc cFirstAmpLoc);

SWIFT_NAME("BridgedDictionaryTypeRepr.createParsed(_:leftSquareLoc:keyType:"
           "colonLoc:valueType:rightSquareLoc:)")
BridgedTypeRepr DictionaryTypeRepr_createParsed(BridgedASTContext cContext,
                                                BridgedSourceLoc cLSquareLoc,
                                                BridgedTypeRepr keyType,
                                                BridgedSourceLoc cColonloc,
                                                BridgedTypeRepr valueType,
                                                BridgedSourceLoc cRSquareLoc);

SWIFT_NAME("BridgedFunctionTypeRepr.createParsed(_:argsType:asyncLoc:throwsLoc:"
           "thrownType:arrowLoc:resultType:)")
BridgedTypeRepr FunctionTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr argsTy,
    BridgedSourceLoc cAsyncLoc, BridgedSourceLoc cThrowsLoc,
    BridgedNullableTypeRepr thrownType, BridgedSourceLoc cArrowLoc,
    BridgedTypeRepr resultType);

SWIFT_NAME("BridgedGenericIdentTypeRepr.createParsed(_:name:nameLoc:"
           "genericArgs:leftAngleLoc:rightAngleLoc:)")
BridgedTypeRepr GenericIdentTypeRepr_createParsed(BridgedASTContext cContext,
                                                  BridgedIdentifier name,
                                                  BridgedSourceLoc cNameLoc,
                                                  BridgedArrayRef genericArgs,
                                                  BridgedSourceLoc cLAngleLoc,
                                                  BridgedSourceLoc cRAngleLoc);

SWIFT_NAME("BridgedOptionalTypeRepr.createParsed(_:base:questionLoc:)")
BridgedTypeRepr OptionalTypeRepr_createParsed(BridgedASTContext cContext,
                                              BridgedTypeRepr base,
                                              BridgedSourceLoc cQuestionLoc);

SWIFT_NAME("BridgedImplicitlyUnwrappedOptionalTypeRepr.createParsed(_:base:"
           "exclaimLoc:)")
BridgedTypeRepr ImplicitlyUnwrappedOptionalTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base,
    BridgedSourceLoc cExclamationLoc);

SWIFT_NAME("BridgedMemberTypeRepr.createParsed(_:base:members:)")
BridgedTypeRepr
MemberTypeRepr_createParsed(BridgedASTContext cContext,
                            BridgedTypeRepr baseComponent,
                            BridgedArrayRef bridgedMemberComponents);

SWIFT_NAME("BridgedMetatypeTypeRepr.createParsed(_:base:typeKeywordLoc:)")
BridgedTypeRepr MetatypeTypeRepr_createParsed(BridgedASTContext cContext,
                                              BridgedTypeRepr baseType,
                                              BridgedSourceLoc cTypeLoc);

SWIFT_NAME("BridgedProtocolTypeRepr.createParsed(_:base:protocolKeywordLoc:)")
BridgedTypeRepr ProtocolTypeRepr_createParsed(BridgedASTContext cContext,
                                              BridgedTypeRepr baseType,
                                              BridgedSourceLoc cProtoLoc);

SWIFT_NAME(
    "BridgedPackExpansionTypeRepr.createParsed(_:base:repeatKeywordLoc:)")
BridgedTypeRepr PackExpansionTypeRepr_createParsed(BridgedASTContext cContext,
                                                   BridgedTypeRepr base,
                                                   BridgedSourceLoc cRepeatLoc);

SWIFT_NAME(
    "BridgedTupleTypeRepr.createParsed(_:elements:leftParenLoc:rightParenLoc:)")
BridgedTypeRepr TupleTypeRepr_createParsed(BridgedASTContext cContext,
                                           BridgedArrayRef elements,
                                           BridgedSourceLoc cLParenLoc,
                                           BridgedSourceLoc cRParenLoc);

SWIFT_NAME("BridgedNamedOpaqueReturnTypeRepr.createParsed(_:base:)")
BridgedTypeRepr
NamedOpaqueReturnTypeRepr_createParsed(BridgedASTContext cContext,
                                       BridgedTypeRepr baseTy);

SWIFT_NAME("BridgedOpaqueReturnTypeRepr.createParsed(_:someKeywordLoc:base:)")
BridgedTypeRepr OpaqueReturnTypeRepr_createParsed(BridgedASTContext cContext,
                                                  BridgedSourceLoc cOpaqueLoc,
                                                  BridgedTypeRepr baseTy);

SWIFT_NAME("BridgedExistentialTypeRepr.createParsed(_:anyKeywordLoc:base:)")
BridgedTypeRepr ExistentialTypeRepr_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cAnyLoc,
                                                 BridgedTypeRepr baseTy);

SWIFT_NAME("BridgedVarargTypeRepr.createParsed(_:base:ellipsisLoc:)")
BridgedTypeRepr VarargTypeRepr_createParsed(BridgedASTContext cContext,
                                            BridgedTypeRepr base,
                                            BridgedSourceLoc cEllipsisLoc);

SWIFT_NAME("BridgedTopLevelCodeDecl.dump(self:)")
void TopLevelCodeDecl_dump(BridgedTopLevelCodeDecl decl);

SWIFT_NAME("BridgedExpr.dump(self:)")
void Expr_dump(BridgedExpr expr);

SWIFT_NAME("BridgedDecl.dump(self:)")
void Decl_dump(BridgedDecl decl);

SWIFT_NAME("BridgedStmt.dump(self:)")
void Stmt_dump(BridgedStmt statement);

SWIFT_NAME("BridgedTypeRepr.dump(self:)")
void TypeRepr_dump(BridgedTypeRepr type);

//===----------------------------------------------------------------------===//
// Plugins
//===----------------------------------------------------------------------===//

typedef void *PluginHandle;
typedef const void *PluginCapabilityPtr;

/// Set a capability data to the plugin object. Since the data is just a opaque
/// pointer, it's not used in AST at all.
void Plugin_setCapability(PluginHandle handle, PluginCapabilityPtr _Nullable data);

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
_Bool Plugin_spawnIfNeeded(PluginHandle handle);

/// Sends the message to the plugin, returns true if there was an error.
/// Clients should receive the response  by \c Plugin_waitForNextMessage .
_Bool Plugin_sendMessage(PluginHandle handle, const BridgedData data);

/// Receive a message from the plugin.
_Bool Plugin_waitForNextMessage(PluginHandle handle, BridgedData *data);

#ifdef __cplusplus
}
#endif

SWIFT_END_ASSUME_NONNULL
SWIFT_END_NULLABILITY_ANNOTATIONS

#undef SWIFT_NAME

#endif // SWIFT_C_AST_ASTBRIDGING_H
