//===--- TypeInfo.cpp - Type information relevant to SILGen -----*- C++ -*-===//
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

#include "TypeInfo.h"
#include "TypeVisitor.h"

namespace swift {
namespace Lowering {

TypeInfo TypeInfo::get(Type t) {
  if (t->hasReferenceSemantics()) {
    return TypeInfo(/*addressOnly=*/false, /*trivial=*/false);
  } else {
    /* FIXME: walk aggregate types to determine address-only-ness and
     * triviality. */
    return TypeInfo(/*addressOnly=*/false, /*trivial=*/true);
  }
}
  
} // namespace Lowering
} // namespace swift