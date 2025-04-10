//===--- DiagnosticArgument.cpp ---------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticArgument.h"
#include "swift/Basic/Assertions.h"

using namespace swift;

DiagnosticArgument::DiagnosticArgument(StringRef S)
    : Kind(DiagnosticArgumentKind::String), StringVal(S) {}

DiagnosticArgument::DiagnosticArgument(int I)
    : Kind(DiagnosticArgumentKind::Integer), IntegerVal(I) {}

DiagnosticArgument::DiagnosticArgument(unsigned I)
    : Kind(DiagnosticArgumentKind::Unsigned), UnsignedVal(I) {}

DiagnosticArgument::DiagnosticArgument(DeclNameRef R)
    : Kind(DiagnosticArgumentKind::Identifier), IdentifierVal(R) {}

DiagnosticArgument::DiagnosticArgument(DeclName D)
    : Kind(DiagnosticArgumentKind::Identifier), IdentifierVal(DeclNameRef(D)) {}

DiagnosticArgument::DiagnosticArgument(DeclBaseName D)
    : Kind(DiagnosticArgumentKind::Identifier), IdentifierVal(DeclNameRef(D)) {}

DiagnosticArgument::DiagnosticArgument(Identifier I)
    : Kind(DiagnosticArgumentKind::Identifier), IdentifierVal(DeclNameRef(I)) {}

DiagnosticArgument::DiagnosticArgument(ObjCSelector S)
    : Kind(DiagnosticArgumentKind::ObjCSelector), ObjCSelectorVal(S) {}

DiagnosticArgument::DiagnosticArgument(const Decl *VD)
    : Kind(DiagnosticArgumentKind::Decl), TheDecl(VD) {}

DiagnosticArgument::DiagnosticArgument(Type T)
    : Kind(DiagnosticArgumentKind::Type), TypeVal(T) {}

DiagnosticArgument::DiagnosticArgument(TypeRepr *T)
    : Kind(DiagnosticArgumentKind::TypeRepr), TyR(T) {}

DiagnosticArgument::DiagnosticArgument(FullyQualified<Type> FQT)
    : Kind(DiagnosticArgumentKind::FullyQualifiedType),
      FullyQualifiedTypeVal(FQT) {}

DiagnosticArgument::DiagnosticArgument(WitnessType WT)
    : Kind(DiagnosticArgumentKind::WitnessType), WitnessTypeVal(WT) {}

DiagnosticArgument::DiagnosticArgument(const TypeLoc &TL) {
  if (TypeRepr *tyR = TL.getTypeRepr()) {
    Kind = DiagnosticArgumentKind::TypeRepr;
    TyR = tyR;
  } else {
    Kind = DiagnosticArgumentKind::Type;
    TypeVal = TL.getType();
  }
}

DiagnosticArgument::DiagnosticArgument(DescriptivePatternKind DPK)
    : Kind(DiagnosticArgumentKind::DescriptivePatternKind),
      DescriptivePatternKindVal(DPK) {}

DiagnosticArgument::DiagnosticArgument(ReferenceOwnership RO)
    : Kind(DiagnosticArgumentKind::ReferenceOwnership),
      ReferenceOwnershipVal(RO) {}

DiagnosticArgument::DiagnosticArgument(SelfAccessKind SAK)
    : Kind(DiagnosticArgumentKind::SelfAccessKind), SelfAccessKindVal(SAK) {}

DiagnosticArgument::DiagnosticArgument(StaticSpellingKind SSK)
    : Kind(DiagnosticArgumentKind::StaticSpellingKind),
      StaticSpellingKindVal(SSK) {}

DiagnosticArgument::DiagnosticArgument(DescriptiveDeclKind DDK)
    : Kind(DiagnosticArgumentKind::DescriptiveDeclKind),
      DescriptiveDeclKindVal(DDK) {}

DiagnosticArgument::DiagnosticArgument(StmtKind SK)
    : Kind(DiagnosticArgumentKind::DescriptiveStmtKind),
      DescriptiveStmtKindVal(SK) {}

DiagnosticArgument::DiagnosticArgument(const DeclAttribute *attr)
    : Kind(DiagnosticArgumentKind::DeclAttribute), DeclAttributeVal(attr) {}

DiagnosticArgument::DiagnosticArgument(const TypeAttribute *attr)
    : Kind(DiagnosticArgumentKind::TypeAttribute), TypeAttributeVal(attr) {}

DiagnosticArgument::DiagnosticArgument(const AvailabilityDomain domain)
    : Kind(DiagnosticArgumentKind::AvailabilityDomain),
      AvailabilityDomainVal(domain) {}

DiagnosticArgument::DiagnosticArgument(const AvailabilityRange &range)
    : Kind(DiagnosticArgumentKind::AvailabilityRange),
      AvailabilityRangeVal(range) {}

DiagnosticArgument::DiagnosticArgument(llvm::VersionTuple version)
    : Kind(DiagnosticArgumentKind::VersionTuple), VersionVal(version) {}

DiagnosticArgument::DiagnosticArgument(LayoutConstraint L)
    : Kind(DiagnosticArgumentKind::LayoutConstraint), LayoutConstraintVal(L) {}

DiagnosticArgument::DiagnosticArgument(ActorIsolation AI)
    : Kind(DiagnosticArgumentKind::ActorIsolation), ActorIsolationVal(AI) {}

DiagnosticArgument::DiagnosticArgument(IsolationSource IS)
    : Kind(DiagnosticArgumentKind::IsolationSource), IsolationSourceVal(IS) {}

DiagnosticArgument::DiagnosticArgument(DiagnosticInfo *D)
    : Kind(DiagnosticArgumentKind::Diagnostic), DiagnosticVal(D) {}

DiagnosticArgument::DiagnosticArgument(const clang::NamedDecl *ND)
    : Kind(DiagnosticArgumentKind::ClangDecl), ClangDecl(ND) {}

DiagnosticArgument::DiagnosticArgument(const clang::Type *Ty)
    : Kind(DiagnosticArgumentKind::ClangType), ClangType(Ty) {}

DiagnosticArgumentKind DiagnosticArgument::getKind() const { return Kind; }

StringRef DiagnosticArgument::getAsString() const {
  ASSERT(Kind == DiagnosticArgumentKind::String);
  return StringVal;
}

int DiagnosticArgument::getAsInteger() const {
  ASSERT(Kind == DiagnosticArgumentKind::Integer);
  return IntegerVal;
}

unsigned DiagnosticArgument::getAsUnsigned() const {
  ASSERT(Kind == DiagnosticArgumentKind::Unsigned);
  return UnsignedVal;
}

DeclNameRef DiagnosticArgument::getAsIdentifier() const {
  ASSERT(Kind == DiagnosticArgumentKind::Identifier);
  return IdentifierVal;
}

ObjCSelector DiagnosticArgument::getAsObjCSelector() const {
  ASSERT(Kind == DiagnosticArgumentKind::ObjCSelector);
  return ObjCSelectorVal;
}

const Decl *DiagnosticArgument::getAsDecl() const {
  ASSERT(Kind == DiagnosticArgumentKind::Decl);
  return TheDecl;
}

Type DiagnosticArgument::getAsType() const {
  ASSERT(Kind == DiagnosticArgumentKind::Type);
  return TypeVal;
}

TypeRepr *DiagnosticArgument::getAsTypeRepr() const {
  ASSERT(Kind == DiagnosticArgumentKind::TypeRepr);
  return TyR;
}

FullyQualified<Type> DiagnosticArgument::getAsFullyQualifiedType() const {
  ASSERT(Kind == DiagnosticArgumentKind::FullyQualifiedType);
  return FullyQualifiedTypeVal;
}

WitnessType DiagnosticArgument::getAsWitnessType() const {
  ASSERT(Kind == DiagnosticArgumentKind::WitnessType);
  return WitnessTypeVal;
}

DescriptivePatternKind DiagnosticArgument::getAsDescriptivePatternKind() const {
  ASSERT(Kind == DiagnosticArgumentKind::DescriptivePatternKind);
  return DescriptivePatternKindVal;
}

ReferenceOwnership DiagnosticArgument::getAsReferenceOwnership() const {
  ASSERT(Kind == DiagnosticArgumentKind::ReferenceOwnership);
  return ReferenceOwnershipVal;
}

SelfAccessKind DiagnosticArgument::getAsSelfAccessKind() const {
  ASSERT(Kind == DiagnosticArgumentKind::SelfAccessKind);
  return SelfAccessKindVal;
}

StaticSpellingKind DiagnosticArgument::getAsStaticSpellingKind() const {
  ASSERT(Kind == DiagnosticArgumentKind::StaticSpellingKind);
  return StaticSpellingKindVal;
}

DescriptiveDeclKind DiagnosticArgument::getAsDescriptiveDeclKind() const {
  ASSERT(Kind == DiagnosticArgumentKind::DescriptiveDeclKind);
  return DescriptiveDeclKindVal;
}

StmtKind DiagnosticArgument::getAsDescriptiveStmtKind() const {
  ASSERT(Kind == DiagnosticArgumentKind::DescriptiveStmtKind);
  return DescriptiveStmtKindVal;
}

const DeclAttribute *DiagnosticArgument::getAsDeclAttribute() const {
  ASSERT(Kind == DiagnosticArgumentKind::DeclAttribute);
  return DeclAttributeVal;
}

const TypeAttribute *DiagnosticArgument::getAsTypeAttribute() const {
  ASSERT(Kind == DiagnosticArgumentKind::TypeAttribute);
  return TypeAttributeVal;
}

const AvailabilityDomain DiagnosticArgument::getAsAvailabilityDomain() const {
  ASSERT(Kind == DiagnosticArgumentKind::AvailabilityDomain);
  return AvailabilityDomainVal;
}

const AvailabilityRange DiagnosticArgument::getAsAvailabilityRange() const {
  ASSERT(Kind == DiagnosticArgumentKind::AvailabilityRange);
  return AvailabilityRangeVal;
}

llvm::VersionTuple DiagnosticArgument::getAsVersionTuple() const {
  ASSERT(Kind == DiagnosticArgumentKind::VersionTuple);
  return VersionVal;
}

LayoutConstraint DiagnosticArgument::getAsLayoutConstraint() const {
  ASSERT(Kind == DiagnosticArgumentKind::LayoutConstraint);
  return LayoutConstraintVal;
}

ActorIsolation DiagnosticArgument::getAsActorIsolation() const {
  ASSERT(Kind == DiagnosticArgumentKind::ActorIsolation);
  return ActorIsolationVal;
}

IsolationSource DiagnosticArgument::getAsIsolationSource() const {
  ASSERT(Kind == DiagnosticArgumentKind::IsolationSource);
  return IsolationSourceVal;
}

DiagnosticInfo *DiagnosticArgument::getAsDiagnostic() const {
  ASSERT(Kind == DiagnosticArgumentKind::Diagnostic);
  return DiagnosticVal;
}

const clang::NamedDecl *DiagnosticArgument::getAsClangDecl() const {
  ASSERT(Kind == DiagnosticArgumentKind::ClangDecl);
  return ClangDecl;
}

const clang::Type *DiagnosticArgument::getAsClangType() const {
  ASSERT(Kind == DiagnosticArgumentKind::ClangType);
  return ClangType;
}
