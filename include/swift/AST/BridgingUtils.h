//===--- BridgingUtils.h - utilities for swift bridging -------------------===//
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

#ifndef SWIFT_AST_BRIDGINGUTILS_H
#define SWIFT_AST_BRIDGINGUTILS_H

#include "swift/AST/ASTBridging.h"
#include "swift/AST/DiagnosticEngine.h"

namespace swift {

inline BridgedDiagEngine getBridgedDiagnosticEngine(DiagnosticEngine *D) {
  return {(void *)D};
}
inline BridgedOptionalDiagnosticEngine
getBridgedOptionalDiagnosticEngine(DiagnosticEngine *D) {
  return {(void *)D};
}

} // namespace swift

#endif

