//===--- ClassAllocatorWitnesses.h - Class allocator witnesses --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// The value witness table of a class with a custom instance allocator (a
// `ClassAllocator`) has a class allocator witness (see
// TargetClassAllocatorValueWitnessTable), `deallocate`, which frees an
// instance through the allocator. Classes with the same allocator share a
// table.
//
// The entry has the lowering of a `deallocate(_:byteCount:alignment:for:)`
// declared on a non-generic struct or enum: `swiftcc void (allocation, size,
// alignment, metadata)`. If the allocator is such a type, and its witness for
// the requirement is its own method with exactly that lowering, the entry is
// the method itself (getClassAllocatorDeallocateMethod). Otherwise the
// ClassAllocatorWitnesses SIL pass creates a shared SIL function for the
// allocator type, with that signature, that calls the witness. IRGen emits
// the tables, which reference the method or the function. IRGen also calls
// the entry, and the allocate witness that the pass creates, of a class's
// instance allocator directly to allocate and free a stack-promoted instance
// that it can't put on the stack, and to free an instance after its last
// unowned release.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_CLASSALLOCATORWITNESSES_H
#define SWIFT_SIL_CLASSALLOCATORWITNESSES_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/ResilienceExpansion.h"
#include "swift/SIL/SILDeclRef.h"
#include <optional>
#include <string>

namespace swift {

class ConcreteDeclRef;
class SILFunction;
class SILModule;

/// The instance allocator that the class allocator witness in the value
/// witness table of \p cls calls, or null if the table has no class allocator
/// witness. A class's table has one if and only if the class has a custom
/// instance allocator, which its metadata records with
/// ClassFlags::HasClassAllocator.
inline CanType getClassAllocatorWitnessType(const ClassDecl *cls) {
  if (Type allocatorType = cls->getAllocatorType())
    return allocatorType->getCanonicalType();
  return CanType();
}

/// Whether the concurrency runtime, rather than compiled code, frees the
/// instances of \p cls, an actor class with a class allocator, from the
/// deallocating deinit: a default actor that is still running a job when its
/// deinit finishes is freed when the job ends. \p module is the module being
/// compiled.
inline bool isActorFreedByConcurrencyRuntime(const ClassDecl *cls,
                                             ModuleDecl *module) {
  auto *rootActorClass = cls->getRootActorClass();
  if (!rootActorClass)
    return false;
  if (rootActorClass->isDefaultActor(module, ResilienceExpansion::Maximal))
    return true;
  // It may be a default actor.
  return !rootActorClass->isForeign() &&
         rootActorClass->isResilient(module, ResilienceExpansion::Maximal);
}

enum class ClassAllocatorWitnessKind {
  /// `(metadata, size, alignMask) -> allocation`, calling a `ClassAllocator`'s
  /// `allocate(byteCount:alignment:for:)`; for stack-promoted instances that
  /// IRGen can't put on the stack.
  Allocate,
  /// `(allocation, size, alignment, metadata)`, calling a `ClassAllocator`'s
  /// `deallocate(_:byteCount:alignment:for:)`: the `deallocate` entry of the
  /// value witness tables of the allocator's classes, unless the entry is the
  /// allocator's own method (getClassAllocatorDeallocateMethod).
  Deallocate,
};

/// The name of the SIL function that calls \p allocatorType's requirement
/// \p kind as a class allocator witness.
inline std::string
getClassAllocatorWitnessName(CanType allocatorType,
                             ClassAllocatorWitnessKind kind) {
  std::string name;
  switch (kind) {
  case ClassAllocatorWitnessKind::Allocate:
    name = "__swift_class_allocator_allocate_";
    break;
  case ClassAllocatorWitnessKind::Deallocate:
    name = "__swift_class_allocator_deallocate_";
    break;
  }
  Mangle::ASTMangler mangler(allocatorType->getASTContext());
  name += mangler.mangleTypeWithoutPrefix(allocatorType);
  return name;
}

/// The `deallocate(_:byteCount:alignment:for:)` requirement of
/// `ClassAllocator`, or null if the standard library doesn't have it.
FuncDecl *getClassAllocatorDeallocateRequirement(ASTContext &ctx);

/// The witness of \p allocatorType's conformance to `ClassAllocator` for
/// `deallocate(_:byteCount:alignment:for:)`, with its substitutions, if code in
/// \p M may call it with a `function_ref` rather than through the
/// conformance: it's synchronous, doesn't throw, isn't actor-isolated,
/// `dynamic`, back-deployed or dispatched dynamically, and its symbol is
/// visible from \p M. Otherwise null.
ConcreteDeclRef getDirectlyCallableClassAllocatorDeallocate(SILModule &M,
                                                            CanType allocatorType);

/// If \p allocatorType is a non-generic struct or enum whose witness for
/// `deallocate(_:byteCount:alignment:for:)` is a method declared on the type
/// itself, callable directly (see getDirectlyCallableClassAllocatorDeallocate),
/// whose lowered type is exactly
/// `@convention(method) (UnsafeMutableRawPointer, Int, Int,
/// @thick any AnyObject.Type, @thin A.Type) -> ()`, that method: the `deallocate`
/// entry of the value witness tables of the allocator's classes is its
/// address, `swiftcc void (ptr, i64, i64, ptr)`, as the thin metatype has no
/// representation. Otherwise the entry is the function that
/// ClassAllocatorWitnesses creates.
std::optional<SILDeclRef> getClassAllocatorDeallocateMethod(SILModule &M,
                                                            CanType allocatorType);

/// The function that the `deallocate` entry of the value witness tables of
/// \p allocatorType's classes is, and that IRGen calls to free an instance
/// through the allocator: the function that ClassAllocatorWitnesses created,
/// or the allocator's method (getClassAllocatorDeallocateMethod). Null if
/// \p M has neither, or only a declaration of a function without a symbol.
SILFunction *lookUpClassAllocatorDeallocateEntry(SILModule &M,
                                                 CanType allocatorType);

/// Whether code in \p fn's module may reference \p fn: it's a definition,
/// or a declaration of a function that has a symbol (unlike an
/// `@_alwaysEmitIntoClient` function, whose body each module that uses it
/// emits).
bool isReferenceableClassAllocatorFunction(const SILFunction *fn);

} // end namespace swift

#endif
