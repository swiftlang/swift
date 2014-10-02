//===- Optional.h - Simple variant for passing optional values --*- C++ -*-===//
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
//  This file provides Optional, a template class modeled in the spirit of
//  OCaml's 'opt' variant.  The idea is to strongly type whether or not
//  a value can be optional.
//
//  Note that this is a C++11-only re-implementation of LLVM's Optional class,
//  which provides the benefit of not constructing the object. This could also
//  be implemented in C++98/03 for LLVM (and will eventually be), but it's a
//  pain to re-implement unrestricted unions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_OPTIONAL_H
#define SWIFT_BASIC_OPTIONAL_H

#include "llvm/ADT/Optional.h"

namespace swift {
  static auto Nothing = llvm::None;
  using Nothing_t = decltype(Nothing);

  template <typename T>
  using Optional = llvm::Optional<T>;
}

#endif
