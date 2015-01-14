//===--- TypeAlignments.h - Alignments of various Swift types ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the alignment of various Swift AST classes.
//
// It's useful to do this in a dedicated place to avoid recursive header
// problems.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPEALIGNMENTS_H
#define SWIFT_TYPEALIGNMENTS_H

namespace swift {
  class AbstractStorageDecl;
  class Decl;
  class DeclContext;
  class OperatorDecl;
  class ArchetypeType;
  class TypeBase;

  /// We frequently use three tag bits on all of these types.
  constexpr size_t DeclAlignInBits = 3;
  constexpr size_t DeclContextAlignInBits = 3;
  constexpr size_t TypeAlignInBits = 3;
}

namespace llvm {
  /// Helper class for declaring the expected alignment of a pointer.
  /// TODO: LLVM should provide this.
  template <class T, size_t AlignInBits> struct MoreAlignedPointerTraits {
    enum { NumLowBitsAvailable = AlignInBits };
    static inline void *getAsVoidPointer(T *ptr) { return ptr; }
    static inline T *getFromVoidPointer(void *ptr) {
      return static_cast<T*>(ptr);
    }
  };

  template <class T> class PointerLikeTypeTraits;
}

/// Declare the expected alignment of pointers to the given type.
/// This macro should be invoked from a top-level file context.
#define LLVM_DECLARE_TYPE_ALIGNMENT(CLASS, ALIGNMENT)     \
namespace llvm {                                          \
template <> class PointerLikeTypeTraits<CLASS*>           \
  : public MoreAlignedPointerTraits<CLASS, ALIGNMENT> {}; \
}

LLVM_DECLARE_TYPE_ALIGNMENT(swift::Decl, swift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::AbstractStorageDecl, swift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::OperatorDecl, swift::DeclAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::DeclContext, swift::DeclContextAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::TypeBase, swift::TypeAlignInBits)
LLVM_DECLARE_TYPE_ALIGNMENT(swift::ArchetypeType, swift::TypeAlignInBits)

#endif
