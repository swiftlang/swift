//===--- Semantics.h - Semantics Attribute Definitions -------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implementation of the matching definition file.
// This file holds all semantics attributes as constant string literals.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMANTICS_H
#define SWIFT_SEMANTICS_H

#include "llvm/ADT/StringRef.h"

namespace swift {
#define SEMA_ATTR(NAME, C_STR) constexpr static const StringLiteral NAME = #C_STR;
#include "SemanticAttrs.def"
}

#endif
