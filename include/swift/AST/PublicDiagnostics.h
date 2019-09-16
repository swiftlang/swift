//===--- PublicDiagnostics.h - Public Diagnostics Index ---------*- C++ -*-===//
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
//  Public diagnostics correspond to one or more internal diagnostics and have
//  a stable, unique identifier.
//
//===----------------------------------------------------------------------===//

#ifndef PublicDiagnostics_h
#define PublicDiagnostics_h

namespace swift {

enum class DiagID : uint32_t;

enum class PublicDiagnosticID : uint32_t {
#define PUBLICDIAG(ID, ADDEDIN, REMOVEDIN) ID,
#include "swift/AST/PublicDiagnostics.def"
};

struct PublicDiagnostic {
  const char *Name;
  const char *AddedInVersion;
  const char *RemovedInVersion;

  constexpr PublicDiagnostic(const char *Name, const char *AddedInVersion,
                             const char *RemovedInVersion)
      : Name(Name), AddedInVersion(AddedInVersion),
        RemovedInVersion(RemovedInVersion) {}

  static PublicDiagnostic forID(PublicDiagnosticID ID);

  static const Optional<PublicDiagnostic> forInternalID(DiagID id);
};

static const constexpr PublicDiagnostic publicDiagnostics[] = {
#define PUBLICDIAG(ID, ADDEDIN, REMOVEDIN)                                     \
  PublicDiagnostic(#ID, ADDEDIN, REMOVEDIN),
#include "swift/AST/PublicDiagnostics.def"
};

static const constexpr PublicDiagnosticID internalToPublicIDs[] = {
#define DIAG(KIND, ID, Options, Text, Signature) PublicDiagnosticID::none,
#define PUBLICERROR(PUBLICID, ID, Options, Text, Signature)                    \
  PublicDiagnosticID::PUBLICID,
#define PUBLICWARNING(PUBLICID, ID, Options, Text, Signature)                  \
  PublicDiagnosticID::PUBLICID,
#define PUBLICNOTE(PUBLICID, ID, Options, Text, Signature)                     \
  PublicDiagnosticID::PUBLICID,
#include "swift/AST/DiagnosticsAll.def"
};

inline PublicDiagnostic PublicDiagnostic::forID(PublicDiagnosticID ID) {
  return publicDiagnostics[(uint32_t)ID];
}

inline const Optional<PublicDiagnostic>
PublicDiagnostic::forInternalID(DiagID id) {
  auto publicID = internalToPublicIDs[(uint32_t)id];
  if (!(uint32_t)publicID)
    return None;
  return PublicDiagnostic::forID(publicID);
}

} // end namespace swift

#endif /* PublicDiagnostics_h */
