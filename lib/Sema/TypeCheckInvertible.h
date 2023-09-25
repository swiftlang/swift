#ifndef SWIFT_SEMA_TYPECHECKINVERTIBLE_H
#define SWIFT_SEMA_TYPECHECKINVERTIBLE_H

#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/ProtocolConformance.h"

namespace swift {


// Is this type copyable in the given decl context?
bool isCopyable(Type type, DeclContext *dc);
bool isNoncopyable(Type type, DeclContext *dc);

// Without checking the generic context, is this type sometimes noncopyable?
bool canBeNoncopyable(Type type);





/// You should be using `ImplicitKnownProtocolConformanceRequest` instead
ProtocolConformance *deriveConformanceForInvertible(Evaluator &evaluator,
                                                    NominalTypeDecl *nominal,
                                                    KnownProtocolKind kp);

}


#endif // SWIFT_SEMA_TYPECHECKINVERTIBLE_H
