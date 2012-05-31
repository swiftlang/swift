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
#include "swift/AST/NameLookup.h"

using namespace swift;

static Type getInstanceUsageType(ValueDecl *Value, ASTContext &Context) {
  Type Ty = Value->getType();
  if (FuncDecl *Func = dyn_cast<FuncDecl>(Value)) {
    // FIXME: Revisit when we add 'this' to static functions.
    if (!Func->isStatic()) {
      if (auto FuncTy = dyn_cast<FunctionType>(Func->getType()))
        return FuncTy->getResult()->getUnlabeledType(Context);
    }
  }
  
  return Ty->getUnlabeledType(Context);
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
  llvm::DenseMap<ArchetypeType *, Type> TypeMapping;
  llvm::DenseMap<ProtocolDecl *, ProtocolConformance *> InheritedMapping;

  // Check that T conforms to all inherited protocols.
  for (auto Inherited : Proto->getInherited()) {
    SmallVector<ProtocolDecl *, 4> InheritedProtos;
    if (!Inherited->isExistentialType(InheritedProtos))
      return nullptr;
    
    for (auto InheritedProto : InheritedProtos) {
      if (auto Conformance = TC.conformsToProtocol(T, InheritedProto,
                                                   ComplainLoc))
        InheritedMapping[InheritedProto] = Conformance;
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
    
    // Bind the implicit 'This' type to the type T.
    // FIXME: Should have some kind of 'implicit' bit to detect this.
    if (AssociatedType->getName().str().equals("This")) {
      TypeMapping[AssociatedType->getUnderlyingType()->getAs<ArchetypeType>()]
        = T;
      continue;
    }
    
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
        case MemberLookupResult::ExistentialMember:
          // Fall-through
          
        case MemberLookupResult::TupleElement:
          // Tuple elements cannot satisfy requirements.
          break;
        }
      }
      
      if (Viable.size() == 1) {
        TypeMapping[AssociatedType->getUnderlyingType()->getAs<ArchetypeType>()]
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
        
        TypeMapping[AssociatedType->getUnderlyingType()->getAs<ArchetypeType>()]
          = ErrorType::get(TC.Context);
        continue;
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
      
      TypeMapping[AssociatedType->getUnderlyingType()->getAs<ArchetypeType>()]
        = ErrorType::get(TC.Context);
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
      // Determine the type that we require the witness to have, substituting
      // in the witnesses we've collected for our archetypes.
      Type RequiredTy = TC.substType(getInstanceUsageType(Requirement,
                                                          TC.Context),
                                     TypeMapping);
      
      for (auto Candidate : Lookup.Results) {
        switch (Candidate.Kind) {
        case MemberLookupResult::MetatypeMember:
          // Static members are ignored.
          // FIXME: Diagnose if static members happen to match?
          break;
            
        case MemberLookupResult::MemberProperty:
        case MemberLookupResult::MemberFunction:
        case MemberLookupResult::ExistentialMember:
          if (Candidate.D->getKind() == Requirement->getKind() &&
              RequiredTy->isEqual(getInstanceUsageType(Candidate.D,
                                                       TC.Context)))
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
                      getInstanceUsageType(Candidate, TC.Context));
        
        continue;
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
                  getInstanceUsageType(Requirement, TC.Context));
      for (auto Candidate : Lookup.Results) {
        if (Candidate.hasDecl())
          TC.diagnose(Candidate.D->getLocStart(), diag::protocol_witness,
                      getInstanceUsageType(Candidate.D, TC.Context));
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
