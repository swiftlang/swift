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

#include "swift/Basic/BasicBridging.h"
#include "swift/Basic/Compiler.h"
#include <stdbool.h>
#include <stddef.h>

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

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
typedef enum ENUM_EXTENSIBILITY_ATTR(open) BridgedDiagID : uint32_t {
#define DIAG(KIND, ID, Options, Text, Signature) BridgedDiagID_##ID,
#include "swift/AST/DiagnosticsAll.def"
} BridgedDiagID;

typedef enum ENUM_EXTENSIBILITY_ATTR(open) BridgedDiagnosticArgumentKind {
  BridgedDiagnosticArgumentKind_StringRef,
  BridgedDiagnosticArgumentKind_Int,
} BridgedDiagnosticArgumentKind;

typedef struct {
  BridgedDiagnosticArgumentKind kind;
  union {
    BridgedStringRef stringRefValue;
    SwiftInt intValue;
  } value;
} BridgedDiagnosticArgument;

typedef struct {
  swift::SourceLoc start;
  SwiftInt byteLength;
  BridgedStringRef text;
} BridgedDiagnosticFixIt;

typedef struct {
  void * _Nonnull object;
} BridgedDiagnosticEngine;

typedef struct {
  void *_Nullable object;
} BridgedOptionalDiagnosticEngine;

// FIXME: Can we bridge InFlightDiagnostic?
void DiagnosticEngine_diagnose(BridgedDiagnosticEngine, swift::SourceLoc loc,
                               BridgedDiagID diagID, BridgedArrayRef arguments,
                               BridgedCharSourceRange highlight,
                               BridgedArrayRef fixIts);

bool DiagnosticEngine_hadAnyError(BridgedDiagnosticEngine);

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif // SWIFT_AST_ASTBRIDGING_H
