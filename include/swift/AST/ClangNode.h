//===--- ClangNode.h - How Swift tracks imported Clang entities -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CLANGNODE_H
#define SWIFT_CLANGNODE_H

#include "swift/Basic/Debug.h"
#include "llvm/ADT/PointerUnion.h"

namespace clang {
  class Decl;
  class MacroInfo;
  class ModuleMacro;
  class Module;
  class SourceLocation;
  class SourceRange;
}

namespace swift {
  
namespace detail {
  /// A wrapper to avoid having to import Clang headers. We can't just
  /// forward-declare their PointerLikeTypeTraits because we don't own
  /// the types.
  template <typename T>
  struct ClangNodeBox {
    const T * const value;

    ClangNodeBox() : value{nullptr} {}
    /*implicit*/ ClangNodeBox(const T *V) : value(V) {}

    explicit operator bool() const { return value; }
  };  
}

/// Represents a clang declaration, macro, or module. A macro definition
/// imported from a module is recorded as the ModuleMacro, and a macro
/// defined locally is represented by the MacroInfo.
class ClangNode {
  template <typename T>
  using Box = detail::ClangNodeBox<T>;

  llvm::PointerUnion<Box<clang::Decl>, Box<clang::MacroInfo>,
                     Box<clang::ModuleMacro>, Box<clang::Module>> Ptr;

public:
  ClangNode() = default;
  ClangNode(const clang::Decl *D) : Ptr(D) {}
  ClangNode(const clang::MacroInfo *MI) : Ptr(MI) {}
  ClangNode(const clang::ModuleMacro *MM) : Ptr(MM) {}
  ClangNode(const clang::Module *Mod) : Ptr(Mod) {}

  bool isNull() const { return Ptr.isNull(); }
  explicit operator bool() const { return !isNull(); }

  const clang::Decl *getAsDecl() const {
    return Ptr.dyn_cast<Box<clang::Decl>>().value;
  }
  const clang::MacroInfo *getAsMacroInfo() const {
    return Ptr.dyn_cast<Box<clang::MacroInfo>>().value;
  }
  const clang::ModuleMacro *getAsModuleMacro() const {
    return Ptr.dyn_cast<Box<clang::ModuleMacro>>().value;
  }
  const clang::Module *getAsModule() const {
    return Ptr.dyn_cast<Box<clang::Module>>().value;
  }

  const clang::Decl *castAsDecl() const {
    return Ptr.get<Box<clang::Decl>>().value;
  }
  const clang::MacroInfo *castAsMacroInfo() const {
    return Ptr.get<Box<clang::MacroInfo>>().value;
  }
  const clang::ModuleMacro *castAsModuleMacro() const {
    return Ptr.get<Box<clang::ModuleMacro>>().value;
  }
  const clang::Module *castAsModule() const {
    return Ptr.get<Box<clang::Module>>().value;
  }

  // Get the MacroInfo for a local definition, one imported from a
  // ModuleMacro, or null if it's neither.
  const clang::MacroInfo *getAsMacro() const;

  /// Returns the module either the one wrapped directly, the one from a
  /// clang::ImportDecl or null if it's neither.
  const clang::Module *getClangModule() const;

  /// Returns the owning clang module of this node, if it exists.
  const clang::Module *getOwningClangModule() const;

  clang::SourceLocation getLocation() const;
  clang::SourceRange getSourceRange() const;

  SWIFT_DEBUG_DUMP;

  void *getOpaqueValue() const { return Ptr.getOpaqueValue(); }
  static inline ClangNode getFromOpaqueValue(void *VP) {
    ClangNode N;
    N.Ptr = decltype(Ptr)::getFromOpaqueValue(VP);
    return N;
  }
};

} // end namespace swift

namespace llvm {
template <typename T>
struct PointerLikeTypeTraits<swift::detail::ClangNodeBox<T>> {
  using Box = ::swift::detail::ClangNodeBox<T>;

  static inline void *getAsVoidPointer(Box P) {
    return const_cast<void *>(static_cast<const void *>(P.value));
  }
  static inline Box getFromVoidPointer(const void *P) {
    return Box{static_cast<const T *>(P)};
  }

  /// Note: We are relying on Clang nodes to be at least 4-byte aligned.
  enum { NumLowBitsAvailable = 2 };
};
  
} // end namespace llvm

#endif
