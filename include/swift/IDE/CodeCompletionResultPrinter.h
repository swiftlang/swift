//===--- CodeCompletionResultPrinter.h --------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_CODECOMPLETIONRESULTPRINTER_H
#define SWIFT_IDE_CODECOMPLETIONRESULTPRINTER_H

#include "llvm/Support/raw_ostream.h"

namespace swift {
namespace ide {

class CodeCompletionResult;

void printCodeCompletionResultDescription(const CodeCompletionResult &Result,
                                          llvm::raw_ostream &OS,
                                          bool leadingPunctuation);

void printCodeCompletionResultDescriptionAnnotated(
    const CodeCompletionResult &Result, llvm::raw_ostream &OS,
    bool leadingPunctuation);

void printCodeCompletionResultTypeName(
    const CodeCompletionResult &Result, llvm::raw_ostream &OS);

void printCodeCompletionResultTypeNameAnnotated(
    const CodeCompletionResult &Result, llvm::raw_ostream &OS);

void printCodeCompletionResultSourceText(
    const CodeCompletionResult &Result, llvm::raw_ostream &OS);

void printCodeCompletionResultFilterName(
    const CodeCompletionResult &Result, llvm::raw_ostream &OS);

} // namespace ide
} // namespace swift

#endif
