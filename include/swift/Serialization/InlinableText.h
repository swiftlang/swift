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

#ifndef SWIFT_SERIALIZATION_INLINABLETEXT_H
#define SWIFT_SERIALIZATION_INLINABLETEXT_H

#include "llvm/ADT/StringRef.h"

namespace swift {
class ParamDecl;

/// Extracts the text of the default argument attached to the provided
/// ParamDecl, removing all inactive #if clauses and providing only the
/// text of active #if clauses.
///
/// For example, the default argument:
/// ```
/// {
///   #if false
///   print("false")
///   #else
///   print("true")
///   #endif
/// }
/// ```
/// will return
/// ```
/// print("true")
/// ```
StringRef extractDefaultArgumentText(const ParamDecl *PD,
                                     SmallVectorImpl<char> &scratch);

} // end namespace swift

#endif
