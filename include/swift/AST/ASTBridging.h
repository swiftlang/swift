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

// The name must not collide with BridgedDiagnosticEngine in CASTBridging.h.
struct BridgedDiagEngine {
  void * _Nonnull object;
};

struct BridgedOptionalDiagnosticEngine {
  void *_Nullable object;
};

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
void DiagnosticEngine_diagnose(BridgedDiagEngine, BridgedSourceLoc loc,
                               BridgedDiagID diagID, BridgedArrayRef arguments,
                               BridgedSourceLoc highlightStart, uint32_t hightlightLength,
                               BridgedArrayRef fixIts);

bool DiagnosticEngine_hadAnyError(BridgedDiagEngine);

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif // SWIFT_AST_ASTBRIDGING_H
