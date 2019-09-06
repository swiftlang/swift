//===----- IDERequests.h - IDE functionality Requests -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines IDE request using the evaluator model.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_IDE_REQUESTS_H
#define SWIFT_IDE_REQUESTS_H

#include "swift/AST/ASTTypeIDs.h"
#include "swift/AST/Evaluator.h"
#include "swift/AST/SimpleRequest.h"
#include "swift/IDE/Utils.h"
#include "swift/IDE/IDETypeIDs.h"

namespace swift {
//----------------------------------------------------------------------------//
// Cusor info
//----------------------------------------------------------------------------//

// Input for CursorInfoRequest.
// Putting the source file and location together allows us to print the request
// input well e.g. file.swift:3:4
struct CursorInfoOwner {
  SourceFile *File;
  SourceLoc Loc;

  CursorInfoOwner(SourceFile *File, SourceLoc Loc): File(File), Loc(Loc) { }

  friend llvm::hash_code hash_value(const CursorInfoOwner &CI) {
    return hash_combine(hash_value(CI.File),
      hash_value(CI.Loc.getOpaquePointerValue()));
  }

  friend bool operator==(const CursorInfoOwner &lhs, const CursorInfoOwner &rhs) {
    return lhs.File == rhs.File &&
      lhs.Loc.getOpaquePointerValue() == rhs.Loc.getOpaquePointerValue();
  }

  friend bool operator!=(const CursorInfoOwner &lhs, const CursorInfoOwner &rhs) {
    return !(lhs == rhs);
  }
  bool isValid() const {
    return File && File->getBufferID() && Loc.isValid();
  }
};

void simple_display(llvm::raw_ostream &out, const CursorInfoOwner &owner);

/// Resolve cursor info at a given location.
class CursorInfoRequest:
    public SimpleRequest<CursorInfoRequest,
                         ide::ResolvedCursorInfo(CursorInfoOwner),
                         CacheKind::Cached>
{
public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  llvm::Expected<ide::ResolvedCursorInfo> evaluate(Evaluator &evaluator,
    CursorInfoOwner CI) const;

public:
  // Caching
  bool isCached() const { return true; }
  // Source location
  SourceLoc getNearestLoc() const;
};

//----------------------------------------------------------------------------//
// Range info
//----------------------------------------------------------------------------//

// Input for RangeInfoRequest.
// Putting the source file and location together allows us to print the request
// input well e.g. file.swift:3:4
struct RangeInfoOwner {
  SourceFile *File;
  SourceLoc StartLoc;
  SourceLoc EndLoc;

  RangeInfoOwner(SourceFile *File, SourceLoc StartLoc, SourceLoc EndLoc):
    File(File), StartLoc(StartLoc), EndLoc(EndLoc) {}
  RangeInfoOwner(SourceFile *File, unsigned Offset, unsigned Length);

  friend llvm::hash_code hash_value(const RangeInfoOwner &CI) {
    return hash_combine(hash_value(CI.File),
                        hash_value(CI.StartLoc.getOpaquePointerValue()),
                        hash_value(CI.EndLoc.getOpaquePointerValue()));
  }

  friend bool operator==(const RangeInfoOwner &lhs, const RangeInfoOwner &rhs) {
    return lhs.File == rhs.File && lhs.StartLoc == rhs.StartLoc &&
      lhs.EndLoc == rhs.EndLoc;
  }

  friend bool operator!=(const RangeInfoOwner &lhs, const RangeInfoOwner &rhs) {
    return !(lhs == rhs);
  }

  bool isValid() const {
    return File && File->getBufferID() && StartLoc.isValid() && EndLoc.isValid();
  }
};

void simple_display(llvm::raw_ostream &out, const RangeInfoOwner &owner);

/// Resolve cursor info at a given location.
class RangeInfoRequest:
    public SimpleRequest<RangeInfoRequest,
                         ide::ResolvedRangeInfo(RangeInfoOwner),
                         CacheKind::Cached>
{
public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  llvm::Expected<ide::ResolvedRangeInfo> evaluate(Evaluator &evaluator,
    RangeInfoOwner CI) const;

public:
  // Caching
  bool isCached() const { return true; }
  // Source location
  SourceLoc getNearestLoc() const;
};

//----------------------------------------------------------------------------//
// ProvideDefaultImplForRequest
//----------------------------------------------------------------------------//
/// Collect all the protocol requirements that a given declaration can
///   provide default implementations for. Input is a declaration in extension
///   declaration. The result is an array of requirements.
class ProvideDefaultImplForRequest:
    public SimpleRequest<ProvideDefaultImplForRequest,
                         ArrayRef<ValueDecl*>(ValueDecl*),
                         CacheKind::Cached>
{
public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  llvm::Expected<ArrayRef<ValueDecl*>> evaluate(Evaluator &evaluator,
    ValueDecl* VD) const;

public:
  // Caching
  bool isCached() const { return true; }
  // Source location
  SourceLoc getNearestLoc() const { return SourceLoc(); };
};

//----------------------------------------------------------------------------//
// CollectOverriddenDeclsRequest
//----------------------------------------------------------------------------//
struct OverridenDeclsOwner {
  ValueDecl *VD;
  bool IncludeProtocolRequirements;
  bool Transitive;

  OverridenDeclsOwner(ValueDecl *VD, bool IncludeProtocolRequirements,
    bool Transitive): VD(VD),
      IncludeProtocolRequirements(IncludeProtocolRequirements),
      Transitive(Transitive) {}

  friend llvm::hash_code hash_value(const OverridenDeclsOwner &CI) {
    return hash_combine(hash_value(CI.VD),
                        hash_value(CI.IncludeProtocolRequirements),
                        hash_value(CI.Transitive));
  }

  friend bool operator==(const OverridenDeclsOwner &lhs,
                         const OverridenDeclsOwner &rhs) {
    return lhs.VD == rhs.VD &&
      lhs.IncludeProtocolRequirements == rhs.IncludeProtocolRequirements &&
      lhs.Transitive == rhs.Transitive;
  }

  friend bool operator!=(const OverridenDeclsOwner &lhs,
                         const OverridenDeclsOwner &rhs) {
    return !(lhs == rhs);
  }

  friend void simple_display(llvm::raw_ostream &out,
                             const OverridenDeclsOwner &owner) {
    simple_display(out, owner.VD);
  }
};

/// Get decls that the given decl overrides, protocol requirements that
///   it serves as a default implementation of, and optionally protocol
///   requirements it satisfies in a conforming class
class CollectOverriddenDeclsRequest:
    public SimpleRequest<CollectOverriddenDeclsRequest,
                         ArrayRef<ValueDecl*>(OverridenDeclsOwner),
                         CacheKind::Cached> {
public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  llvm::Expected<ArrayRef<ValueDecl*>> evaluate(Evaluator &evaluator,
    OverridenDeclsOwner Owner) const;

public:
  // Caching
  bool isCached() const { return true; }
  // Source location
  SourceLoc getNearestLoc() const { return SourceLoc(); };
};

//----------------------------------------------------------------------------//
// ResolveProtocolNameRequest
//----------------------------------------------------------------------------//
struct ProtocolNameOwner {
  DeclContext *DC;
  StringRef Name;
  ProtocolNameOwner(DeclContext *DC, StringRef Name): DC(DC), Name(Name) {}

  friend llvm::hash_code hash_value(const ProtocolNameOwner &CI) {
    return hash_value(CI.Name);
  }

  friend bool operator==(const ProtocolNameOwner &lhs,
                         const ProtocolNameOwner &rhs) {
    return lhs.Name == rhs.Name;
  }

  friend bool operator!=(const ProtocolNameOwner &lhs,
                         const ProtocolNameOwner &rhs) {
    return !(lhs == rhs);
  }

  friend void simple_display(llvm::raw_ostream &out,
                             const ProtocolNameOwner &owner) {
    out << "Resolve " << owner.Name << " from ";
    simple_display(out, owner.DC);
  }
};

/// Resolve a protocol name to the protocol decl pointer inside the ASTContext
class ResolveProtocolNameRequest:
    public SimpleRequest<ResolveProtocolNameRequest,
                         ProtocolDecl*(ProtocolNameOwner),
                         CacheKind::Cached>
{
public:
  using SimpleRequest::SimpleRequest;

private:
  friend SimpleRequest;

  // Evaluation.
  llvm::Expected<ProtocolDecl*> evaluate(Evaluator &evaluator,
    ProtocolNameOwner Input) const;

public:
  // Caching
  bool isCached() const { return true; }
  // Source location
  SourceLoc getNearestLoc() const { return SourceLoc(); };
};

/// The zone number for the IDE.
#define SWIFT_TYPEID_ZONE IDE
#define SWIFT_TYPEID_HEADER "swift/IDE/IDERequestIDZone.def"
#include "swift/Basic/DefineTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER

// Set up reporting of evaluated requests.
#define SWIFT_REQUEST(Zone, RequestType, Sig, Caching, LocOptions)             \
template<>                                                                     \
inline void reportEvaluatedRequest(UnifiedStatsReporter &stats,                \
                            const RequestType &request) {                      \
  ++stats.getFrontendCounters().RequestType;                                   \
}
#include "swift/IDE/IDERequestIDZone.def"
#undef SWIFT_REQUEST

} // end namespace swift

#endif // SWIFT_IDE_REQUESTS_H
