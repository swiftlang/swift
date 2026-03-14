#ifndef SWIFT_SEMA_TYPECHECKBITWISE_H
#define SWIFT_SEMA_TYPECHECKBITWISE_H

#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/TypeCheckRequests.h"

namespace swift {
class ProtocolConformance;
class NominalTypeDecl;

/// Check that \p conformance, a conformance of some nominal type to
/// BitwiseCopyable, is valid.
bool checkBitwiseCopyableConformance(ProtocolConformance *conformance,
                                     bool isImplicit);

/// Produce an implicit conformance of \p nominal to BitwiseCopyable if it is
/// valid to do so.
ProtocolConformance *
deriveImplicitBitwiseCopyableConformance(NominalTypeDecl *nominal);
} // end namespace swift

#endif // SWIFT_SEMA_TYPECHECKBITWISE_H
