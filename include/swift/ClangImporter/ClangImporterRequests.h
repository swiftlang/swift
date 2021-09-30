//===--- ClangImporterRequests.h - Clang Importer Requests ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines clang-importer requests.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_CLANG_IMPORTER_REQUESTS_H
#define SWIFT_CLANG_IMPORTER_REQUESTS_H

#include "swift/AST/SimpleRequest.h"
#include "swift/AST/ASTTypeIDs.h"
#include "swift/AST/EvaluatorDependencies.h"

namespace swift {

#define SWIFT_TYPEID_ZONE ClangImporter
#define SWIFT_TYPEID_HEADER "swift/ClangImporter/ClangImporterTypeIDZone.def"
#include "swift/Basic/DefineTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER

// Set up reporting of evaluated requests.
template<typename Request>
void reportEvaluatedRequest(UnifiedStatsReporter &stats,
                            const Request &request);

#define SWIFT_REQUEST(Zone, RequestType, Sig, Caching, LocOptions)             \
  template <>                                                                  \
  inline void reportEvaluatedRequest(UnifiedStatsReporter &stats,              \
                                     const RequestType &request) {             \
    ++stats.getFrontendCounters().RequestType;                                 \
  }
#include "swift/ClangImporter/ClangImporterTypeIDZone.def"
#undef SWIFT_REQUEST

} // end namespace swift

#endif // SWIFT_CLANG_IMPORTER_REQUESTS_H

