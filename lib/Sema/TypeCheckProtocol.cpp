//===--- TypeCheckProtocol.cpp - Protocol Checking ------------------------===//
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
// This file implements semantic analysis for protocols, in particular, checking
// whether a given type conforms to a given protocol.
//===----------------------------------------------------------------------===//
#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "NameLookup.h"
using namespace swift;

static Type getInstanceUsageType(ValueDecl *Value) {
  Type Ty = Value->getType();
  if (FuncDecl *Func = dyn_cast<FuncDecl>(Value)) {
    if (!Func->isStatic())
      return Ty->getAs<FunctionType>()->getResult();
  }
  
  return Ty;
}

static std::unique_ptr<ProtocolConformance>
checkConformsToProtocol(TranslationUnit &TU, Type T, ProtocolDecl *Proto) {
  llvm::DenseMap<ValueDecl *, ValueDecl *> Mapping;
  
  for (auto Member : Proto->getMembers()) {
    auto Requirement = dyn_cast<ValueDecl>(Member);
    if (!Requirement)
      continue;
    
    
    MemberLookup Lookup(T, Requirement->getName(), TU);

    if (Lookup.isSuccess()) {
      SmallVector<ValueDecl *, 2> Viable;
      Type RequiredTy = getInstanceUsageType(Requirement);
      
      for (auto Candidate : Lookup.Results) {
        switch (Candidate.Kind) {
        case MemberLookupResult::IgnoreBase:
          // Static members are ignored.
          // FIXME: Diagnose if static members happen to match?
          break;
            
        case MemberLookupResult::PassBase:
          if (Candidate.D->getKind() == Requirement->getKind() &&
              RequiredTy->isEqual(getInstanceUsageType(Candidate.D)))
            Viable.push_back(Candidate.D);
          break;

        case MemberLookupResult::StructElement:
        case MemberLookupResult::TupleElement:
          // Tuple elements cannot satisfy requirements.
          break;
        }
      }
      
      if (Viable.size() == 1) {
        Mapping[Requirement] = Viable.front();
        continue;
      }
      
      // FIXME: Diagnose ambiguity or lack of matches.
      return nullptr;
    }
    
    // FIXME: Diagnose complete lack of matches.
    return nullptr;
  }
  
  std::unique_ptr<ProtocolConformance> Result(new ProtocolConformance);
  // FIXME: Make DenseMap movable to make this efficient.
  Result->Mapping = std::move(Mapping);
  return Result;
}

ProtocolConformance *
TypeChecker::conformsToProtocol(Type T, ProtocolDecl *Proto) {
  ASTContext::ConformsToMap::key_type Key(T->getCanonicalType(), Proto);
  ASTContext::ConformsToMap::iterator Known = Context.ConformsTo.find(Key);
  if (Known == Context.ConformsTo.end())
    Known = Context.ConformsTo.insert(
              std::make_pair(Key, checkConformsToProtocol(TU, T, Proto))).first;
  return Known->second.get();
}
