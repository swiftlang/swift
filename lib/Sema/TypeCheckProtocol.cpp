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

/// \brief Retrieve the kind of requirement described by the given declaration,
/// for use in some diagnostics.
/// FIXME: Enumify this.
int getRequirementKind(ValueDecl *VD) {
  if (isa<FuncDecl>(VD))
    return 0;
  
  if (isa<VarDecl>(VD))
    return 1;
  
  assert(isa<SubscriptDecl>(VD) && "Unhandled requirement kind");
  return 2;
}

static std::unique_ptr<ProtocolConformance>
checkConformsToProtocol(TypeChecker &TC, Type T, ProtocolDecl *Proto,
                        SourceLoc ComplainLoc) {
  llvm::DenseMap<ValueDecl *, ValueDecl *> Mapping;
  llvm::DenseMap<Type, Type> TypeMapping;
  llvm::DenseMap<ProtocolDecl *, ProtocolConformance *> InheritedMapping;

  // Check that T conforms to all inherited protocols.
  for (auto Inherited : Proto->getInherited()) {
    ProtocolType *InheritedProto = Inherited->getAs<ProtocolType>();
    if (!InheritedProto)
      return nullptr;
    
    if (auto Conformance = TC.conformsToProtocol(T, InheritedProto->getDecl(),
                                                 ComplainLoc))
      InheritedMapping[InheritedProto->getDecl()] = Conformance;
    else {
      // Recursive call already diagnosed this problem, but tack on a note
      // to establish the relationship.
      if (ComplainLoc.isValid()) {
        TC.diagnose(Proto->getLocStart(),
                    diag::inherited_protocol_does_not_conform, T, Inherited);
      }
      return nullptr;
    }
  }
  
  bool Complained = false;
  
  // First, resolve any associated type members. They'll be used for checking
  // other members.
  
  // FIXME: This algorithm is totally busted. We want to allow deduction of
  // associated type witnesses from the parameter/return types of other
  // witnesses, or to figure out an associated type based on one of its other
  // names, which means we need a rather more sophisticated algorithm.
  for (auto Member : Proto->getMembers()) {
    auto AssociatedType = dyn_cast<TypeAliasDecl>(Member);
    if (!AssociatedType)
      continue;
    
    MemberLookup Lookup(T, AssociatedType->getName(), TC.TU);
    if (Lookup.isSuccess()) {
      SmallVector<TypeDecl *, 2> Viable;
      
      for (auto Candidate : Lookup.Results) {
        switch (Candidate.Kind) {
          case MemberLookupResult::MetatypeMember:
            // FIXME: Check this type against the protocol requirements.
            if (auto Type = dyn_cast<TypeDecl>(Candidate.D))
              Viable.push_back(Type);
            break;
            
          case MemberLookupResult::MemberProperty:
          case MemberLookupResult::MemberFunction:
            // Fall-through
            
          case MemberLookupResult::TupleElement:
            // Tuple elements cannot satisfy requirements.
            break;
        }
      }
      
      if (Viable.size() == 1) {
        TypeMapping[AssociatedType->getUnderlyingType()]
          = Viable.front()->getDeclaredType();
        continue;
      }
      
      if (ComplainLoc.isInvalid())
        return nullptr;
      
      if (!Viable.empty()) {
        if (!Complained) {
          TC.diagnose(ComplainLoc, diag::type_does_not_conform,
                      T, Proto->getDeclaredType());
          Complained = true;
        }
        
        TC.diagnose(AssociatedType->getLocStart(),
                    diag::ambiguous_witnesses_type,
                    AssociatedType->getName());
        
        for (auto Candidate : Viable)
          TC.diagnose(Candidate->getLocStart(), diag::protocol_witness_type);
      }
    }
    
    if (ComplainLoc.isValid()) {
      if (!Complained) {
        TC.diagnose(ComplainLoc, diag::type_does_not_conform,
                    T, Proto->getDeclaredType());
        Complained = true;
      }
      
      TC.diagnose(AssociatedType->getLocStart(), diag::no_witnesses_type,
                  AssociatedType->getName());
      for (auto Candidate : Lookup.Results) {
        if (Candidate.hasDecl())
          TC.diagnose(Candidate.D->getLocStart(), diag::protocol_witness_type);
      }
    } else {
      return nullptr;
    }

  }

  // Check that T provides all of the required func/variable/subscript members.
  for (auto Member : Proto->getMembers()) {
    auto Requirement = dyn_cast<ValueDecl>(Member);
    if (!Requirement)
      continue;

    // Associated type requirements handled above.
    if (isa<TypeAliasDecl>(Requirement))
      continue;
    
    // Variable/function/subscript requirements.
    MemberLookup Lookup(T, Requirement->getName(), TC.TU);

    if (Lookup.isSuccess()) {
      SmallVector<ValueDecl *, 2> Viable;
      Type RequiredTy = getInstanceUsageType(Requirement);
      
      for (auto Candidate : Lookup.Results) {
        switch (Candidate.Kind) {
        case MemberLookupResult::MetatypeMember:
          // Static members are ignored.
          // FIXME: Diagnose if static members happen to match?
          break;
            
        case MemberLookupResult::MemberProperty:
        case MemberLookupResult::MemberFunction:
          if (Candidate.D->getKind() == Requirement->getKind() &&
              RequiredTy->isEqual(getInstanceUsageType(Candidate.D)))
            Viable.push_back(Candidate.D);
          break;

        case MemberLookupResult::TupleElement:
          // Tuple elements cannot satisfy requirements.
          break;
        }
      }
      
      if (Viable.size() == 1) {
        Mapping[Requirement] = Viable.front();
        continue;
      }
      
      if (ComplainLoc.isInvalid())
        return nullptr;
      
      if (!Viable.empty()) {
        if (!Complained) {
          TC.diagnose(ComplainLoc, diag::type_does_not_conform,
                      T, Proto->getDeclaredType());
          Complained = true;
        }
        
        TC.diagnose(Requirement->getLocStart(), diag::ambiguous_witnesses,
                    getRequirementKind(Requirement),
                    Requirement->getName(),
                    RequiredTy);
        
        for (auto Candidate : Viable)
          TC.diagnose(Candidate->getLocStart(), diag::protocol_witness,
                      getInstanceUsageType(Candidate));
      }
    }

    if (ComplainLoc.isValid()) {
      if (!Complained) {
        TC.diagnose(ComplainLoc, diag::type_does_not_conform,
                    T, Proto->getDeclaredType());
        Complained = true;
      }

      TC.diagnose(Requirement->getLocStart(), diag::no_witnesses,
                  getRequirementKind(Requirement),
                  Requirement->getName(),
                  getInstanceUsageType(Requirement));
      for (auto Candidate : Lookup.Results) {
        if (Candidate.hasDecl())
          TC.diagnose(Candidate.D->getLocStart(), diag::protocol_witness,
                      getInstanceUsageType(Candidate.D));
      }
    } else {
      return nullptr;
    }
  }
  
  if (Complained)
    return nullptr;
  
  std::unique_ptr<ProtocolConformance> Result(new ProtocolConformance);
  // FIXME: Make DenseMap movable to make this efficient.
  Result->Mapping = std::move(Mapping);
  Result->TypeMapping = std::move(TypeMapping);
  Result->InheritedMapping = std::move(InheritedMapping);
  return Result;
}

ProtocolConformance *
TypeChecker::conformsToProtocol(Type T, ProtocolDecl *Proto,
                                SourceLoc ComplainLoc) {
  ASTContext::ConformsToMap::key_type Key(T->getCanonicalType(), Proto);
  ASTContext::ConformsToMap::iterator Known = Context.ConformsTo.find(Key);
  if (Known != Context.ConformsTo.end())
    return Known->second;
  
  // Assume that the type does not conform to this protocol while checking
  // whether it does in fact conform. This eliminates both infinite recursion
  // (if the protocol hierarchies are circular) as well as tautologies.
  Context.ConformsTo[Key] = nullptr;
  if (std::unique_ptr<ProtocolConformance> Conformance
        = checkConformsToProtocol(*this, T, Proto, ComplainLoc)) {
    auto result = Conformance.release();
    Context.ConformsTo[Key] = result;
    return result;
  }
  return nullptr;
}
