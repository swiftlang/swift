//===--- AbsoluteRawSyntax.cpp ----------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Syntax/AbsoluteRawSyntax.h"

using namespace swift;
using namespace swift::syntax;

std::atomic<SyntaxIdentifier::RootIdType> SyntaxIdentifier::NextUnusedRootId(0);

raw_ostream &llvm::operator<<(raw_ostream &OS,
                              swift::syntax::AbsoluteOffsetPosition Pos) {
  OS << "Offset " << Pos.getOffset();
  return OS;
}
