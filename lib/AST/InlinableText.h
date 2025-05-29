//===------ InlinableText.h - Extract inlinable source text -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_INLINABLETEXT_H
#define SWIFT_AST_INLINABLETEXT_H

#include "swift/AST/ASTNode.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {
class ASTContext;

/// Extracts the text of this ASTNode from the source buffer, ignoring
/// all #if declarations and clauses except the elements that are active.
StringRef extractInlinableText(ASTContext &ctx, ASTNode node,
                               SmallVectorImpl<char> &scratch);

} // end namespace swift

#endif // defined(SWIFT_AST_INLINABLETEXT_H)
