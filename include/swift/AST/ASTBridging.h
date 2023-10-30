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
  class DiagnosticArgument;
  class DiagnosticEngine;
  class NominalTypeDecl;
  class VarDecl;
}

//===----------------------------------------------------------------------===//
// Diagnostic Engine
//===----------------------------------------------------------------------===//

// NOTE: This must be the same underlying value as C++ 'swift::DiagID' defined
// in 'DiagnosticList.cpp'.
typedef enum ENUM_EXTENSIBILITY_ATTR(open) BridgedDiagID : uint32_t {
#define DIAG(KIND, ID, Options, Text, Signature) BridgedDiagID_##ID,
#include "swift/AST/DiagnosticsAll.def"
} BridgedDiagID;

BRIDGING_WRAPPER_NONNULL(DiagnosticEngine)
BRIDGING_WRAPPER_NULLABLE(DiagnosticEngine)

class BridgedDiagnosticArgument {
  int64_t storage[3];

public:
#ifdef USED_IN_CPP_SOURCE
  BridgedDiagnosticArgument(const swift::DiagnosticArgument &arg) {
    *reinterpret_cast<swift::DiagnosticArgument *>(&storage) = arg;
  }
  const swift::DiagnosticArgument &get() const {
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
  const swift::DiagnosticInfo::FixIt &get() const {
    return *reinterpret_cast<const swift::DiagnosticInfo::FixIt *>(&storage);
  }
#endif

  BridgedDiagnosticFixIt(BridgedSourceLoc start, uint32_t length, BridgedStringRef text);
};

// FIXME: Can we bridge InFlightDiagnostic?
void DiagnosticEngine_diagnose(BridgedDiagnosticEngine, BridgedSourceLoc loc,
                               BridgedDiagID diagID, BridgedArrayRef arguments,
                               BridgedSourceLoc highlightStart,
                               uint32_t hightlightLength,
                               BridgedArrayRef fixIts);

bool DiagnosticEngine_hadAnyError(BridgedDiagnosticEngine);

//===----------------------------------------------------------------------===//
// NominalTypeDecl
//===----------------------------------------------------------------------===//

class BridgedNominalTypeDecl {
  swift::NominalTypeDecl * _Nonnull Ptr;

public:
#ifdef USED_IN_CPP_SOURCE
  BridgedNominalTypeDecl(swift::NominalTypeDecl * _Nonnull ptr) : Ptr(ptr) {}

  swift::NominalTypeDecl * _Nonnull get() const { return Ptr; }
#endif
};

SWIFT_NAME("BridgedNominalTypeDecl.getName(self:)")
BRIDGED_INLINE
BridgedStringRef BridgedNominalTypeDecl_getName(BridgedNominalTypeDecl decl);

SWIFT_NAME("BridgedNominalTypeDecl.isStructWithUnreferenceableStorage(self:)")
bool BridgedNominalTypeDecl_isStructWithUnreferenceableStorage(
    BridgedNominalTypeDecl decl);

SWIFT_NAME("BridgedNominalTypeDecl.isGlobalActor(self:)")
BRIDGED_INLINE
bool BridgedNominalTypeDecl_isGlobalActor(BridgedNominalTypeDecl decl);

//===----------------------------------------------------------------------===//
// VarDecl
//===----------------------------------------------------------------------===//

class BridgedVarDecl {
  swift::VarDecl * _Nonnull Ptr;

public:
#ifdef USED_IN_CPP_SOURCE
  BridgedVarDecl(swift::VarDecl * _Nonnull ptr) : Ptr(ptr) {}

  swift::VarDecl * _Nonnull get() const { return Ptr; }
#endif
};

SWIFT_NAME("BridgedVarDecl.getUserFacingName(self:)")
BRIDGED_INLINE
BridgedStringRef BridgedVarDecl_getUserFacingName(BridgedVarDecl decl);

class BridgedNullableVarDecl {
  swift::VarDecl * _Nullable Ptr;

public:
#ifdef USED_IN_CPP_SOURCE
  BridgedNullableVarDecl(swift::VarDecl * _Nullable ptr) : Ptr(ptr) {}

  swift::VarDecl * _Nullable get() const { return Ptr; }
#endif
};

SWIFT_END_NULLABILITY_ANNOTATIONS

#ifndef PURE_BRIDGING_MODE
// In _not_ PURE_BRIDGING_MODE, briding functions are inlined and therefore
// included in the header file.
#include "ASTBridgingImpl.h"
#endif

#endif // SWIFT_AST_ASTBRIDGING_H
