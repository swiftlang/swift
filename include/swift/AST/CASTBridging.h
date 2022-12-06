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

#ifndef SWIFT_C_AST_ASTBRIDGING_H
#define SWIFT_C_AST_ASTBRIDGING_H

#include "swift/Basic/Compiler.h"
#include <inttypes.h>

#if __clang__
// Provide macros to temporarily suppress warning about the use of
// _Nullable and _Nonnull.
#define SWIFT_BEGIN_NULLABILITY_ANNOTATIONS                                    \
  _Pragma("clang diagnostic push")                                             \
      _Pragma("clang diagnostic ignored \"-Wnullability-extension\"")          \
          _Pragma("clang assume_nonnull begin")

#define SWIFT_END_NULLABILITY_ANNOTATIONS                                      \
  _Pragma("clang diagnostic pop") _Pragma("clang assume_nonnull end")
#else
#define SWIFT_BEGIN_NULLABILITY_ANNOTATIONS
#define SWIFT_END_NULLABILITY_ANNOTATIONS
#define _Nullable
#endif

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

typedef long SwiftInt;
typedef unsigned long SwiftUInt;

typedef struct {
  const void *_Nullable data;
  long numElements;
} BridgedArrayRef;

typedef struct {
  void *start;
  SwiftInt byteLength;
} BridgedCharSourceRange;

typedef void *BridgedIdentifier;

typedef struct {
  BridgedIdentifier _Nullable Name;
  void *_Nullable NameLoc;
  BridgedIdentifier _Nullable SecondName;
  void *_Nullable SecondNameLoc;
  void *_Nullable UnderscoreLoc;
  void *_Nullable ColonLoc;
  void *Type;
  void *_Nullable TrailingCommaLoc;
} BridgedTupleTypeElement;

typedef enum ENUM_EXTENSIBILITY_ATTR(open) BridgedRequirementReprKind : long {
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
  void *_Nullable SeparatorLoc;
  BridgedRequirementReprKind Kind;
  void *FirstType;
  void *SecondType;
  // FIXME: Handle Layout Requirements
} BridgedRequirementRepr;

/// Diagnostic severity when reporting diagnostics.
typedef enum ENUM_EXTENSIBILITY_ATTR(open) BridgedDiagnosticSeverity : long {
  BridgedFatalError,
  BridgedError,
  BridgedWarning,
  BridgedRemark,
  BridgedNote,
} BridgedDiagnosticSeverity;

typedef void* BridgedDiagnostic;

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
BridgedDiagnostic SwiftDiagnostic_create(
    void *diagnosticEngine, BridgedDiagnosticSeverity severity,
    void *_Nullable sourceLoc,
    const uint8_t *_Nullable text, long textLen);

/// Highlight a source range as part of the diagnostic.
void SwiftDiagnostic_highlight(
    BridgedDiagnostic diag, void *_Nullable startLoc, void *_Nullable endLoc);

/// Add a Fix-It to replace a source range as part of the diagnostic.
void SwiftDiagnostic_fixItReplace(
    BridgedDiagnostic diag,
    void *_Nullable replaceStartLoc, void *_Nullable replaceEndLoc,
    const uint8_t *_Nullable newText, long newTextLen);

/// Finish the given diagnostic and emit it.
void SwiftDiagnostic_finish(BridgedDiagnostic diag);

BridgedIdentifier SwiftASTContext_getIdentifier(void *ctx,
                                                const uint8_t *_Nullable str,
                                                long len);

void *SwiftImportDecl_create(void *, void *, void *, char, void *,
                             BridgedArrayRef, BridgedArrayRef);

void *SwiftTopLevelCodeDecl_createStmt(void *ctx, void *DC, void *startLoc,
                                       void *element, void *endLoc);
void *SwiftTopLevelCodeDecl_createExpr(void *ctx, void *DC, void *startLoc,
                                       void *element, void *endLoc);

void *ReturnStmt_create(void *ctx, void *loc, void *_Nullable expr);

void *SwiftSequenceExpr_create(void *ctx, BridgedArrayRef exprs);

void *SwiftTupleExpr_create(void *ctx, void *lparen, BridgedArrayRef subs,
                            BridgedArrayRef names,
                            BridgedArrayRef nameLocs,
                            void *rparen);

void *SwiftFunctionCallExpr_create(void *ctx, void *fn, void *args);

void *SwiftIdentifierExpr_create(void *ctx, BridgedIdentifier base, void *loc);

void *SwiftStringLiteralExpr_create(void *ctx, const uint8_t *_Nullable string,
                                    long len, void *TokenLoc);

void *SwiftIntegerLiteralExpr_create(void *ctx, const uint8_t *_Nullable string,
                                     long len, void *TokenLoc);

void *SwiftBooleanLiteralExpr_create(void *ctx, _Bool value, void *TokenLoc);

void *ArrayExpr_create(void *ctx, void *lLoc, BridgedArrayRef elements,
                       BridgedArrayRef commas, void *rLoc);

void *SwiftVarDecl_create(void *ctx, BridgedIdentifier _Nullable name,
                          void *initExpr, void *loc, _Bool isStatic,
                          _Bool isLet, void *dc);

void *IfStmt_create(void *ctx, void *ifLoc, void *cond, void *_Nullable then,
                    void *_Nullable elseLoc, void *_Nullable elseStmt);

typedef enum ENUM_EXTENSIBILITY_ATTR(open) ASTNodeKind : long {
  ASTNodeKindExpr,
  ASTNodeKindStmt,
  ASTNodeKindDecl
} ASTNodeKind;

struct ASTNodeBridged {
  void *ptr;
  ASTNodeKind kind;
};

void *BraceStmt_create(void *ctx, void *lbloc, BridgedArrayRef elements,
                       void *rbloc);

void *BridgedSourceLoc_advanced(void *loc, long len);

void *ParamDecl_create(void *ctx, void *loc, void *_Nullable argLoc,
                       void *_Nullable argName, void *_Nullable paramLoc,
                       void *_Nullable paramName, void *_Nullable type,
                       void *declContext);
struct FuncDeclBridged {
  void *declContext;
  void *funcDecl;
  void *decl;
};

struct FuncDeclBridged
FuncDecl_create(void *ctx, void *staticLoc, _Bool isStatic, void *funcLoc,
                BridgedIdentifier name, void *nameLoc, _Bool isAsync,
                void *_Nullable asyncLoc, _Bool throws,
                void *_Nullable throwsLoc, void *paramLLoc,
                BridgedArrayRef params, void *paramRLoc,
                void *_Nullable returnType, void *declContext);
void FuncDecl_setBody(void *fn, void *body);

void *SimpleIdentTypeRepr_create(void *ctx, void *loc, BridgedIdentifier id);

void *UnresolvedDotExpr_create(void *ctx, void *base, void *dotLoc,
                               BridgedIdentifier name, void *nameLoc);

void *ClosureExpr_create(void *ctx, void *body, void *dc);

void NominalTypeDecl_setMembers(void *decl, BridgedArrayRef members);

struct DeclContextAndDecl {
  void *declContext;
  void *nominalDecl;
  void *decl;
};

struct DeclContextAndDecl
StructDecl_create(void *ctx, void *loc, BridgedIdentifier name, void *nameLoc,
                  void *_Nullable genericParams, void *dc);
struct DeclContextAndDecl ClassDecl_create(void *ctx, void *loc,
                                           BridgedIdentifier name,
                                           void *nameLoc, void *dc);

void *ArrayTypeRepr_create(void *ctx, void *base, void *lsquareLoc,
                           void *rsquareLoc);
void *DictionaryTypeRepr_create(void *ctx, void *keyType, void *valueType,
                                void *lsquareLoc, void *colonloc,
                                void *rsquareLoc);
void *OptionalTypeRepr_create(void *ctx, void *base, void *questionLoc);
void *ImplicitlyUnwrappedOptionalTypeRepr_create(void *ctx, void *base,
                                                 void *exclamationLoc);
void *MetatypeTypeRepr_create(void *ctx, void *baseType, void *typeLoc);
void *ProtocolTypeRepr_create(void *ctx, void *baseType, void *protoLoc);
void *PackExpansionTypeRepr_create(void *ctx, void *base, void *ellipsisLoc);
void *TupleTypeRepr_create(void *ctx, BridgedArrayRef elements, void *lParenLoc,
                           void *rParenLoc);
void *IdentTypeRepr_create(void *ctx, BridgedArrayRef components);
void *GenericIdentTypeRepr_create(void *ctx, BridgedIdentifier name,
                                  void *nameLoc, BridgedArrayRef genericArgs,
                                  void *lAngle, void *rAngle);
void *CompositionTypeRepr_create(void *ctx, BridgedArrayRef types,
                                 void *firstTypeLoc);
void *FunctionTypeRepr_create(void *ctx, void *argsTy, void *_Nullable asyncLoc,
                              void *_Nullable throwsLoc, void *arrowLoc,
                              void *returnType);
void *NamedOpaqueReturnTypeRepr_create(void *ctx, void *baseTy);
void *OpaqueReturnTypeRepr_create(void *ctx, void *opaqueLoc, void *baseTy);
void *ExistentialTypeRepr_create(void *ctx, void *anyLoc, void *baseTy);
void *GenericParamList_create(void *ctx, void *lAngleLoc,
                              BridgedArrayRef params, void *_Nullable whereLoc,
                              BridgedArrayRef reqs, void *rAngleLoc);
void *GenericTypeParamDecl_create(void *ctx, void *declContext,
                                  BridgedIdentifier name, void *nameLoc,
                                  void *_Nullable ellipsisLoc, long index,
                                  _Bool isParameterPack);
void GenericTypeParamDecl_setInheritedType(void *ctx, void *Param, void *ty);

struct DeclContextAndDecl TypeAliasDecl_create(void *ctx, void *declContext,
                                               void *aliasLoc, void *equalLoc,
                                               BridgedIdentifier name,
                                               void *nameLoc,
                                               void *_Nullable genericParams);
void TypeAliasDecl_setUnderlyingTypeRepr(void *decl, void *underlyingType);

void TopLevelCodeDecl_dump(void *);
void Expr_dump(void *);
void Decl_dump(void *);
void Stmt_dump(void *);
void Type_dump(void *);

#ifdef __cplusplus
}
#endif

SWIFT_END_NULLABILITY_ANNOTATIONS

#undef SWIFT_BEGIN_NULLABILITY_ANNOTATIONS
#undef SWIFT_END_NULLABILITY_ANNOTATIONS

#endif // SWIFT_C_AST_ASTBRIDGING_H
