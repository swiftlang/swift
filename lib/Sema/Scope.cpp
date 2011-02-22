//===--- Scope.cpp - Scope Implementation ---------------------------------===//
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
//  This file implements semantic analysis for Swift declarations.
//
//===----------------------------------------------------------------------===//

#include "swift/Sema/Scope.h"
#include "swift/Sema/SemaDecl.h"
using namespace swift;

Scope::Scope(SemaDecl &S) : SD(S),
   ValueHTScope(*(ValueScopeHTTy*)S.ValueScopeHT),
   TypeHTScope(*(TypeScopeHTTy*)S.TypeScopeHT),
   PrevScope(SD.CurScope) {
  if (SD.CurScope)
    Depth = SD.CurScope->Depth+1;
  else
    Depth = 0;
  SD.CurScope = this;
}

Scope::~Scope() {
  assert(SD.CurScope == this && "Scope mismatch");
  SD.CurScope = PrevScope;
}
