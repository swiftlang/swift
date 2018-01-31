//===--- UnknownSyntax.cpp - Swift Unknown  Syntax Implementation ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Syntax/TokenSyntax.h"
#include "swift/Syntax/UnknownSyntax.h"

using namespace swift;
using namespace swift::syntax;

void UnknownSyntax::validate() const {
  assert(Data->getRaw()->isUnknown());
}

