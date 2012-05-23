//===--- TypeChecker.cpp - Type Checking ----------------------------------===//
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
// This file implements semantic analysis for expressions, and other pieces
// that require final type checking.  If this passes a translation unit with no
// errors, then it is good to go.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
using namespace swift;

ProtocolDecl *TypeChecker::getAnyProtocol(SourceLoc Loc) {
  if (!AnyProto) {
    SmallVector<ValueDecl *, 1> Values;
    TU.lookupGlobalValue(Context.getIdentifier("Any"),
                         NLKind::QualifiedLookup, Values);
    if (Values.size() == 1)
      AnyProto = dyn_cast<ProtocolDecl>(Values.front());
  }
  
  if (!AnyProto) {
    // FIXME: Ugly hack. We shouldn't try to do this.
    AnyProto = new (Context) ProtocolDecl(&TU, Loc, Loc,
                                          Context.getIdentifier("Any"),
                                          MutableArrayRef<Type>());
    llvm::SmallVector<Decl *, 1> Members;
    Members.push_back(new (Context) TypeAliasDecl(Loc,
                                                  Context.getIdentifier("This"),
                                                  Type(),  AnyProto));
    AnyProto->setMembers(Context.AllocateCopy(Members),
                         SourceRange(Loc, Loc));
    AnyProto->setDeclaredType(ProtocolType::getNew(AnyProto));
    AnyProto->setType(MetaTypeType::get(AnyProto));
    typeCheckDecl(AnyProto, true);
  }
  
  return AnyProto;
}

ProtocolDecl *TypeChecker::getRangeProtocol() {
  if (!RangeProto) {
    SmallVector<ValueDecl *, 1> Values;
    TU.lookupGlobalValue(Context.getIdentifier("Range"),
                         NLKind::QualifiedLookup, Values);
    if (Values.size() != 1)
      return nullptr;
    
    RangeProto = dyn_cast<ProtocolDecl>(Values.front());
  }
  
  return RangeProto;
}
