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

/// The zone number for the IDE.
#define SWIFT_IDE_REQUESTS_TYPEID_ZONE 137
#define SWIFT_TYPEID_ZONE SWIFT_IDE_REQUESTS_TYPEID_ZONE
#define SWIFT_TYPEID_HEADER "swift/IDE/IDERequestIDZone.def"
#include "swift/Basic/DefineTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER

// Set up reporting of evaluated requests.
#define SWIFT_TYPEID(RequestType)                                \
template<>                                                       \
inline void reportEvaluatedRequest(UnifiedStatsReporter &stats,  \
                            const RequestType &request) {        \
  ++stats.getFrontendCounters().RequestType;                     \
}
#include "swift/IDE/IDERequestIDZone.def"
#undef SWIFT_TYPEID

} // end namespace swift

#endif // SWIFT_IDE_REQUESTS_H
