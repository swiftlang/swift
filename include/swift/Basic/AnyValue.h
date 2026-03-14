//===--- AnyValue.h ---------------------------------------------*- C++ -*-===//
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
//
// This file defines some miscellaneous overloads of hash_value() and
// simple_display().
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_ANYVALUE_H
#define SWIFT_BASIC_ANYVALUE_H

#include "swift/Basic/SimpleDisplay.h"
#include "swift/Basic/TypeID.h"
#include "llvm/ADT/PointerUnion.h"  // to define hash_value
#include "llvm/ADT/TinyPtrVector.h"

namespace llvm {
  // FIXME: Belongs in LLVM itself
  template<typename PT1, typename PT2>
  hash_code hash_value(const llvm::PointerUnion<PT1, PT2> &ptr) {
    return hash_value(ptr.getOpaqueValue());
  }

  template<typename T>
  bool operator==(const TinyPtrVector<T> &lhs, const TinyPtrVector<T> &rhs) {
    if (lhs.size() != rhs.size())
      return false;
    
    for (unsigned i = 0, n = lhs.size(); i != n; ++i) {
      if (lhs[i] != rhs[i])
        return false;
    }
    
    return true;
  }
  
  template<typename T>
  bool operator!=(const TinyPtrVector<T> &lhs, const TinyPtrVector<T> &rhs) {
    return !(lhs == rhs);
  }

  template <typename T>
  void simple_display(raw_ostream &out, const std::optional<T> &opt) {
    if (opt) {
      simple_display(out, *opt);
    } else {
      out << "None";
    }
  }
} // end namespace llvm

#endif //


