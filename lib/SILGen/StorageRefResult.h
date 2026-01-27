#ifndef SWIFT_SILGEN_STORAGEREFRESULT_H
#define SWIFT_SILGEN_STORAGEREFRESULT_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/SIL/SILModule.h"

namespace swift {
/// Container to hold the result of a search for the storage reference
/// when determining to emit a borrow.
struct StorageRefResult {
private:
  Expr *storageRef;
  Expr *transitiveRoot;

public:
  // Represents an empty result
  StorageRefResult() : storageRef(nullptr), transitiveRoot(nullptr) {}
  bool isEmpty() const { return transitiveRoot == nullptr; }
  operator bool() const { return !isEmpty(); }

  /// The root of the expression that accesses the storage in \c storageRef.
  /// When in doubt, this is probably what you want, as it includes the
  /// entire expression tree involving the reference.
  Expr *getTransitiveRoot() const { return transitiveRoot; }

  /// The direct storage reference that was discovered.
  Expr *getStorageRef() const { return storageRef; }

  StorageRefResult(Expr *storageRef, Expr *transitiveRoot)
      : storageRef(storageRef), transitiveRoot(transitiveRoot) {
    assert(storageRef && transitiveRoot && "use the zero-arg init for empty");
  }

  // Initializes a storage reference where the base matches the ref.
  StorageRefResult(Expr *storageRef)
      : StorageRefResult(storageRef, storageRef) {}

  StorageRefResult withTransitiveRoot(StorageRefResult refResult) const {
    return withTransitiveRoot(refResult.transitiveRoot);
  }

  StorageRefResult withTransitiveRoot(Expr *newRoot) const {
    return StorageRefResult(storageRef, newRoot);
  }

  static inline StorageRefResult
  findStorageReferenceExprForBorrow(SILModule &M, Expr *e) {
    e = e->getSemanticsProvidingExpr();

    // These are basically defined as the cases implemented by SILGenLValue.

    // Direct storage references.
    if (auto dre = dyn_cast<DeclRefExpr>(e)) {
      if (isa<VarDecl>(dre->getDecl()))
        return dre;
    } else if (auto mre = dyn_cast<MemberRefExpr>(e)) {
      if (isa<VarDecl>(mre->getDecl().getDecl()))
        return mre;
    } else if (isa<SubscriptExpr>(e)) {
      return e;
    } else if (isa<OpaqueValueExpr>(e)) {
      return e;
    } else if (isa<KeyPathApplicationExpr>(e)) {
      return e;

      // Transitive storage references.  Look through these to see if the
      // sub-expression is a storage reference, but don't return the
      // sub-expression.
    } else if (auto tue = dyn_cast<TupleElementExpr>(e)) {
      if (auto result = findStorageReferenceExprForBorrow(M, tue->getBase()))
        return result.withTransitiveRoot(tue);

    } else if (auto fve = dyn_cast<ForceValueExpr>(e)) {
      if (auto result = findStorageReferenceExprForBorrow(M, fve->getSubExpr()))
        return result.withTransitiveRoot(fve);

    } else if (auto boe = dyn_cast<BindOptionalExpr>(e)) {
      if (auto result = findStorageReferenceExprForBorrow(M, boe->getSubExpr()))
        return result.withTransitiveRoot(boe);

    } else if (auto oe = dyn_cast<OpenExistentialExpr>(e)) {
      if (findStorageReferenceExprForBorrow(M, oe->getExistentialValue()))
        if (auto result = findStorageReferenceExprForBorrow(M, oe->getSubExpr()))
          return result.withTransitiveRoot(oe);

    } else if (auto bie = dyn_cast<DotSyntaxBaseIgnoredExpr>(e)) {
      if (auto result = findStorageReferenceExprForBorrow(M, bie->getRHS()))
        return result.withTransitiveRoot(bie);

    } else if (auto te = dyn_cast<AnyTryExpr>(e)) {
      if (auto result = findStorageReferenceExprForBorrow(M, te->getSubExpr()))
        return result.withTransitiveRoot(te);

    } else if (auto ioe = dyn_cast<InOutExpr>(e)) {
      if (auto result = findStorageReferenceExprForBorrow(M, ioe->getSubExpr()))
        return result.withTransitiveRoot(ioe);
    } else if (auto le = dyn_cast<LoadExpr>(e)) {
      if (auto result = findStorageReferenceExprForBorrow(M, le->getSubExpr()))
        return result.withTransitiveRoot(le);
    } else if (auto ae = dyn_cast<ApplyExpr>(e)) {
      // Some builtins can produce borrows as a special case.
      auto declRef = ae->getFn()->getReferencedDecl();
      if (!declRef) {
        return StorageRefResult();
      }
      if (!declRef.getDecl()->getModuleContext()->isBuiltinModule()) {
        return StorageRefResult();
      }

      auto &builtinInfo
        = M.getBuiltinInfo(declRef.getDecl()->getBaseIdentifier());

      if (builtinInfo.ID != BuiltinValueKind::DereferenceBorrow) {
        return StorageRefResult();
      }

      if (auto result = findStorageReferenceExprForBorrow(M,
                                                  ae->getArgs()->getExpr(0))) {
        return result.withTransitiveRoot(e);
      }
    }

    return StorageRefResult();
  }
};

} // namespace swift

#endif
