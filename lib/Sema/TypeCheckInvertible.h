#ifndef SWIFT_SEMA_TYPECHECKINVERTIBLE_H
#define SWIFT_SEMA_TYPECHECKINVERTIBLE_H

#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/ProtocolConformance.h"

namespace swift {

class StorageVisitor {
public:
  /// Visit the instance storage of the given nominal type as seen through
  /// the given declaration context.
  ///
  /// The `this` instance is invoked with each (stored property, property type)
  /// pair for classes/structs and with each (enum elem, associated value type)
  /// pair for enums.
  ///
  /// Thus, the requirements of your `VisitorImpl` must match up with this
  /// public interface.
  ///
  /// \returns \c true if any call to the \c visitor returns \c true, and
  /// \c false otherwise.
  bool visit(NominalTypeDecl *nominal, DeclContext *dc);

  /// Handle a stored property.
  virtual bool operator()(VarDecl *property, Type propertyType) = 0;

  /// Handle an enum associated value.
  virtual bool operator()(EnumElementDecl *element, Type elementType) = 0;
};


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
