//===--- SourceFileExtras.h - Extra data for a source file ------*- C++ -*-===//
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

#ifndef SWIFT_AST_SOURCEFILEEXTRAS_H
#define SWIFT_AST_SOURCEFILEEXTRAS_H

#include "llvm/ADT/DenseMap.h"
#include <vector>

namespace swift {

class Decl;

/// Extra information associated with a source file that is lazily created and
/// stored in a separately-allocated side structure.
struct SourceFileExtras {
};
  
}

#endif // SWIFT_AST_SOURCEFILEEXTRAS_H
