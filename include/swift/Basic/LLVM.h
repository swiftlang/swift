//===--- LLVM.h - Import various common LLVM datatypes ----------*- C++ -*-===//
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
// This file forward declares and imports various common LLVM datatypes that
// swift wants to use unqualified.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_LLVM_H
#define SWIFT_AST_LLVM_H

#include "llvm/Support/Casting.h"

// Forward declarations.
namespace llvm {
  // Containers
  class StringRef;
  class Twine;
  template <typename T> class SmallVectorImpl;
  template <typename T, unsigned N> class SmallVector;
  template<typename T> class ArrayRef;
  template<typename T> class MutableArrayRef;
  template<typename T> class NullablePtr;
  template<typename T> class TinyPtrVector;
  template <typename PT1, typename PT2> class PointerUnion;
  
  // Other common classes.
  class raw_ostream;
  class APInt;
  class APFloat;
} // end namespace llvm;


namespace swift {
  // Casting operators.
  using llvm::isa;
  using llvm::cast;
  using llvm::dyn_cast;
  using llvm::dyn_cast_or_null;
  using llvm::cast_or_null;

  // Containers
  using llvm::StringRef;
  using llvm::Twine;
  using llvm::SmallVectorImpl;
  using llvm::SmallVector;
  using llvm::ArrayRef;
  using llvm::MutableArrayRef;
  using llvm::NullablePtr;
  using llvm::TinyPtrVector;
  using llvm::PointerUnion;

  // Other common classes.
  using llvm::raw_ostream;
  using llvm::APInt;
  using llvm::APFloat;
} // end namespace swift

#endif
