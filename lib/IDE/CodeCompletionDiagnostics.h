//===--- CodeCompletionDiagnostics.h --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_CODECOMPLETIONDIAGNOSTICS_H
#define SWIFT_IDE_CODECOMPLETIONDIAGNOSTICS_H

#include "swift/IDE/CodeCompletion.h"

namespace swift {

class ValueDecl;

namespace ide {

bool getCompletionDiagnostics(CodeCompletionResult::NotRecommendedReason reason,
                              const ValueDecl *D,
                              CodeCompletionDiagnosticSeverity &severity,
                              llvm::raw_ostream &Out);

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_CODECOMPLETIONDIAGNOSTICS_H
