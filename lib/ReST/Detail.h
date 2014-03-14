//===--- Detail.h - ReST parsing implementation details -------------------===//
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

#ifndef LLVM_REST_DETAIL_H
#define LLVM_REST_DETAIL_H

#include "clang/Basic/CharInfo.h"

namespace llvm {
namespace rest {
namespace detail {

/// According to [ReST/Syntax Details/Whitespace], only four ASCII characters
/// are considered whitespace.
///
/// REST-FIXME: Unicode supports a lot of different whitespace characeters, so
/// limiting processing just to these four ASCII characeters seems wrong.
/// Audit all users of this functions if it is changed to allow other
/// whitespace, also check that those new characters are allowed in
/// indentation.
static inline bool isReSTWhitespace(char C) {
  return clang::isHorizontalWhitespace(C);
}

} // namespace detail
} // namespace rest
} // namespace llvm

#endif // LLVM_REST_DETAIL_H

