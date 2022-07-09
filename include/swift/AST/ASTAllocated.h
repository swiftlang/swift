//===--- ASTAllocated.h - Allocation in the AST Context ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ASTALLOCATED_H
#define SWIFT_AST_ASTALLOCATED_H

#include <cassert>
#include <cstddef>

namespace swift {
class ASTContext;

/// The arena in which a particular ASTContext allocation will go.
enum class AllocationArena {
  /// The permanent arena, which is tied to the lifetime of
  /// the ASTContext.
  ///
  /// All global declarations and types need to be allocated into this arena.
  /// At present, everything that is not a type involving a type variable is
  /// allocated in this arena.
  Permanent,
  /// The constraint solver's temporary arena, which is tied to the
  /// lifetime of a particular instance of the constraint solver.
  ///
  /// Any type involving a type variable is allocated in this arena.
  ConstraintSolver
};

namespace detail {
void *allocateInASTContext(size_t bytes, const ASTContext &ctx,
                           AllocationArena arena, unsigned alignment);
}

/// Types inheriting from this class are intended to be allocated in an
/// \c ASTContext allocator; you cannot allocate them by using a normal \c new,
/// and instead you must either provide an \c ASTContext or use a placement
/// \c new.
///
/// The template parameter is a type with the desired alignment. It is usually,
/// but not always, the type that is inheriting \c ASTAllocated.
template <typename AlignTy>
class ASTAllocated {
public:
  // Make vanilla new/delete illegal.
  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;

  // Only allow allocation using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t bytes, const ASTContext &ctx,
                     AllocationArena arena = AllocationArena::Permanent,
                     unsigned alignment = alignof(AlignTy)) {
    return detail::allocateInASTContext(bytes, ctx, arena, alignment);
  }

  void *operator new(size_t Bytes, void *Mem) throw() {
    assert(Mem && "placement new into failed allocation");
    return Mem;
  }
};

}

#endif
