#include "swift/SILAnalysis/DestructorAnalysis.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/SIL/SILModule.h"
#include "llvm/Support/Debug.h"

#define DEBUG_TYPE "destructor-analysis"

using namespace swift;

/// A type T's destructor does not store to memory if the type
///  * is a trivial builtin type like builtin float or int types
///  * is a value type with stored properties that are safe or
///  * is a value type that implements the _DestructorSafeContainer protocol and
///    whose type parameters are safe types T1...Tn.
bool DestructorAnalysis::mayStoreToMemoryOnDestruction(SILType T) {
  bool IsSafe = isSafeType(T.getSwiftRValueType());
  DEBUG(llvm::dbgs() << " DestructorAnalysis::mayStoreToMemoryOnDestruction is"
                     << (IsSafe ? " false: " : " true: "));
  DEBUG(T.getSwiftRValueType()->print(llvm::errs()));
  DEBUG(llvm::errs() << "\n");
  return !IsSafe;
}

bool DestructorAnalysis::cacheResult(CanType Type, bool Result) {
  Cached[Type] = Result;
  return Result;
}

bool DestructorAnalysis::isSafeType(Type Ty) {
  CanType Canonical = Ty.getCanonicalTypeOrNull();
  if (Canonical.isNull())
    return false;

  // Don't visit types twice.
  auto CachedRes = Cached.find(Canonical);
  if (CachedRes != Cached.end()) {
    return CachedRes->second;
  }

  // Before we recurse mark the type as safe i.e if we see it in a recursive
  // possition it is safe in the absence of another fact that proves otherwise.
  // We will reset this value to the correct value once we return from the
  // recursion below.
  cacheResult(Canonical, true);

  // Trivial value types.
  if (Canonical->getKind() == TypeKind::BuiltinInteger)
    return cacheResult(Canonical, true);
  if (Canonical->getKind() == TypeKind::BuiltinFloat)
    return cacheResult(Canonical, true);

  // A struct is safe if
  //   * either it implements the _DestructorSafeContainer protocol and
  //     all the type parameters are safe types.
  //   * or all stored properties are safe types.
  if (auto *Struct = Canonical->getStructOrBoundGenericStruct()) {

    if (implementsDestructorSafeContainerProtocol(Struct) &&
        areTypeParametersSafe(Canonical))
      return cacheResult(Canonical, true);

    // Check the stored properties.
    for (auto SP : Struct->getStoredProperties())
      if (!isSafeType(SP->getType()))
        return cacheResult(Canonical, false);

    return cacheResult(Canonical, true);
  }

  // A tuple type is safe if its elements are safe.
  if (auto Tuple = dyn_cast<TupleType>(Canonical)) {
    for (auto &Elt : Tuple->getElements())
      if (!isSafeType(Elt.getType()))
        return cacheResult(Canonical, false);
    return cacheResult(Canonical, true);
  }

  // TODO: enum types.

  return cacheResult(Canonical, false);
}

bool DestructorAnalysis::implementsDestructorSafeContainerProtocol(
    NominalTypeDecl *NomDecl) {
  ProtocolDecl *DestructorSafeContainer =
      getASTContext().getProtocol(KnownProtocolKind::_DestructorSafeContainer);

  for (auto Proto : NomDecl->getAllProtocols())
    if (Proto == DestructorSafeContainer)
      return true;

  return false;
}

bool DestructorAnalysis::areTypeParametersSafe(CanType Ty) {
  auto BGT = dyn_cast<BoundGenericType>(Ty);
  if (!BGT)
    return false;

  // Make sure all type parameters are safe.
  for (auto TP : BGT->getGenericArgs()) {
    if (!isSafeType(TP))
      return false;
  }
  return true;
}

ASTContext &DestructorAnalysis::getASTContext() {
  return Mod->getASTContext();
}

SILAnalysis *swift::createDestructorAnalysis(SILModule *M) {
  return new DestructorAnalysis(M);
}
