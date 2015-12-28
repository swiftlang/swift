//===--- Parameter.cpp - Functions & closures parameters ------------------===//
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
//
// This file defines the Parameter class, the ParameterList class and support
// logic.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Parameter.h"
#include "swift/AST/ASTContext.h"
using namespace swift;

/// TODO: unique and reuse the () parameter list in ASTContext, as well as the
/// "(self : T)" parameter lists common to many methods.
ParameterList *
ParameterList::create(ASTContext &context, ArrayRef<Parameter> params) {
  auto byteSize = sizeof(ParameterList)+params.size()*sizeof(Parameter);
  auto rawMem = context.Allocate(byteSize, alignof(ParameterList));
  
  //  Placement initialize the ParameterList and the Parameter's.
  auto PL = ::new (rawMem) ParameterList(params.size());

  for (size_t i = 0, e = params.size(); i != e; ++i)
    ::new (&PL->getParameter(i)) Parameter(params[i]);
  
  return PL;
}
