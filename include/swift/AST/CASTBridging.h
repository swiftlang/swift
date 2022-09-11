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

// Provide macros to temporarily suppress warning about the use of
// _Nullable and _Nonnull.
#define SWIFT_BEGIN_NULLABILITY_ANNOTATIONS                                    \
  _Pragma("clang diagnostic push")                                             \
      _Pragma("clang diagnostic ignored \"-Wnullability-extension\"")          \
          _Pragma("clang assume_nonnull begin")

#define SWIFT_END_NULLABILITY_ANNOTATIONS                                      \
  _Pragma("clang diagnostic pop") _Pragma("clang assume_nonnull end")

//===----------------------------------------------------------------------===//
// Diagnostic Engine
//===----------------------------------------------------------------------===//

// TODO: Move this to somewhere common header.
#if __has_attribute(enum_extensibility)
#define ENUM_EXTENSIBILITY_ATTR(arg) __attribute__((enum_extensibility(arg)))
#else
#define ENUM_EXTENSIBILITY_ATTR(arg)
#endif

// NOTE: This must be the same underlying value as C++ 'swift::DiagID' defined
// in 'DiagnosticList.cpp'.
typedef enum ENUM_EXTENSIBILITY_ATTR(open) BridgedDiagID : unsigned {
#define DIAG(KIND, ID, Options, Text, Signature) BridgedDiagID_##ID,
#include "swift/AST/DiagnosticsAll.def"
} BridgedDiagID;

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

// FIXME: Can we bridge InFlightDiagnostic?
void DiagnosticEngine_diagnose(void *, void *loc, BridgedDiagID diagID,
                               BridgedArrayRef arguments,
                               BridgedCharSourceRange highlight,
                               BridgedArrayRef fixIts);

int DiagnosticEngine_hadAnyError(void *);

typedef const char *BridgedIdentifier;

#ifdef __cplusplus
extern "C" {

#define _Bool bool

#endif

BridgedIdentifier
SwiftASTContext_getIdentifier(void *ctx, const char *_Nullable str, long len);

void *SwiftImportDecl_create(void *, void *, void *, char, void *,
                             BridgedArrayRef, BridgedArrayRef);

void *SwiftTopLevelCodeDecl_createStmt(void *ctx, void *DC, void *startLoc,
                                       void *element, void *endLoc);
void *SwiftTopLevelCodeDecl_createExpr(void *ctx, void *DC, void *startLoc,
                                       void *element, void *endLoc);

void *SwiftSequenceExpr_create(void *ctx, BridgedArrayRef exprs);

void *SwiftTupleExpr_create(void *ctx, void *lparen, BridgedArrayRef subs,
                            void *rparen);

void *SwiftFunctionCallExpr_create(void *ctx, void *fn, void *args);

void *SwiftIdentifierExpr_create(void *ctx, const char *base, void *loc);

void *SwiftStringLiteralExpr_create(void *ctx, const char *_Nullable string,
                                    long len, void *TokenLoc);

void *SwiftIntegerLiteralExpr_create(void *ctx, const char *_Nullable string,
                                    long len, void *TokenLoc);

void *SwiftVarDecl_create(void *ctx, const char *_Nullable name,
                          void *loc, _Bool isStatic, _Bool isLet, void *dc);

void *IfStmt_create(void *ctx, void *ifLoc, void *cond, void *_Nullable then, void *_Nullable elseLoc,
                    void *_Nullable elseStmt);

void *BraceStmt_create(void *ctx, void *lbloc, BridgedArrayRef elements, void *rbloc);

void *BridgedSourceLoc_advanced(void *loc, long len);

void *ParamDecl_create(void *ctx, void *loc,
                       void *_Nullable argLoc, void *_Nullable argName,
                       void *_Nullable paramLoc, void *_Nullable paramName,
                       void *declContext);

void *FuncDecl_create(void *ctx, void *staticLoc, _Bool isStatic, void *funcLoc,
                      const char *name, void *nameLoc,
                      _Bool isAsync, void *_Nullable asyncLoc,
                      _Bool throws, void *_Nullable throwsLoc,
                      void *paramLLoc, BridgedArrayRef params, void *paramRLoc,
                      void *_Nullable body, void *_Nullable returnType,
                      void *declContext);

void *SimpleIdentTypeRepr_create(void *ctx, void *loc, const char *id);

void *UnresolvedDotExpr_create(void *ctx, void *base, void *dotLoc, const char *name, void *nameLoc);

void *ClosureExpr_create(void *ctx, void *body, void *dc);

void NominalTypeDecl_setMembers(void *decl, BridgedArrayRef members);

struct DeclContextAndDecl {
  void *declContext;
  void *nominalDecl;
  void *decl;
};

struct DeclContextAndDecl StructDecl_create(void *ctx, void *loc, const char *name, void *nameLoc,
                        void *dc);
struct DeclContextAndDecl ClassDecl_create(void *ctx, void *loc, const char *name, void *nameLoc,
                       void *dc);

void TopLevelCodeDecl_dump(void *);
void Expr_dump(void *);
void Decl_dump(void *);
void Stmt_dump(void *);

#ifdef __cplusplus
}
#endif

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif // SWIFT_C_AST_ASTBRIDGING_H