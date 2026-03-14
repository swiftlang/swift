//===--- ConformanceLookup.h - Global conformance lookup --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_CONFORMANCELOOKUP_H
#define SWIFT_AST_CONFORMANCELOOKUP_H

#include "llvm/ADT/ArrayRef.h"
#include <optional>

namespace swift {

class CanType;
class Type;
class ProtocolConformanceRef;
class ProtocolDecl;

/// Global conformance lookup, does not check conditional requirements.
///
/// \param type The type for which we are computing conformance.
///
/// \param protocol The protocol to which we are computing conformance.
///
/// \param allowMissing When \c true, the resulting conformance reference
/// might include "missing" conformances, which are synthesized for some
/// protocols as an error recovery mechanism.
///
/// \returns An invalid conformance if the search failed, otherwise an
/// abstract, concrete or pack conformance, depending on the lookup type.
ProtocolConformanceRef lookupConformance(Type type, ProtocolDecl *protocol,
                                         bool allowMissing = false);

/// Global conformance lookup, checks conditional requirements.
/// Requires a contextualized type.
///
/// \param type The type for which we are computing conformance. Must not
/// contain type parameters.
///
/// \param protocol The protocol to which we are computing conformance.
///
/// \param allowMissing When \c true, the resulting conformance reference
/// might include "missing" conformances, which are synthesized for some
/// protocols as an error recovery mechanism.
///
/// \returns An invalid conformance if the search failed, otherwise an
/// abstract, concrete or pack conformance, depending on the lookup type.
ProtocolConformanceRef checkConformance(Type type, ProtocolDecl *protocol,
                                        // Note: different default from
                                        // lookupConformance
                                        bool allowMissing = true);

/// Global conformance lookup, checks conditional requirements.
/// Accepts interface types without context. If the conformance cannot be
/// definitively established without the missing context, returns \c nullopt.
///
/// \param type The type for which we are computing conformance. Must not
/// contain type parameters.
///
/// \param protocol The protocol to which we are computing conformance.
///
/// \param allowMissing When \c true, the resulting conformance reference
/// might include "missing" conformances, which are synthesized for some
/// protocols as an error recovery mechanism.
///
/// \returns An invalid conformance if the search definitively failed. An
/// abstract, concrete or pack conformance, depending on the lookup type,
/// if the search succeeded. `std::nullopt` if the type could have
/// conditionally conformed depending on the context of the interface types.
std::optional<ProtocolConformanceRef>
checkConformanceWithoutContext(Type type,
                               ProtocolDecl *protocol,
                               // Note: different default from
                               // lookupConformance
                               bool allowMissing = true);


/// Look for the conformance of the given existential type to the given
/// protocol.
ProtocolConformanceRef lookupExistentialConformance(Type type,
                                                    ProtocolDecl *protocol);

/// Collect the conformances of \c fromType to each of the protocols of an
/// existential type's layout.
llvm::ArrayRef<ProtocolConformanceRef>
collectExistentialConformances(CanType fromType, CanType existential,
                               bool allowMissing = false);

}

#endif // SWIFT_AST_CONFORMANCELOOKUP_H