//===--- Parser.h - ReST parser -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_REST_PARSER_H
#define LLVM_REST_PARSER_H

#include "swift/ReST/LineList.h"
#include "llvm/ADT/SmallVector.h"

namespace llvm {
namespace rest {
namespace detail {

LineClassification classifyLine(const Line &L);

} // namespace detail

void extractBrief(LineListRef LL, llvm::SmallVectorImpl<char> &Str);

} // namespace rest
} // namespace llvm

#endif // LLVM_REST_PARSER_H

