//===--- TypeWalker.h - Class for walking a Type ----------------*- C++ -*-===//
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

#ifndef SWIFT_AST_TYPEWALKER_H
#define SWIFT_AST_TYPEWALKER_H

#include "swift/AST/Type.h"

namespace swift {

/// \brief An abstract class used to traverse a Type.
class TypeWalker {
public:
  enum class Action {
    Continue,
    SkipChildren,
    Stop
  };

  /// This method is called when first visiting a type before walking into its
  /// children.
  virtual Action walkToTypePre(Type ty) { return Action::Continue; }

  /// This method is called after visiting a type's children.
  virtual Action walkToTypePost(Type ty) { return Action::Continue; }

protected:
  TypeWalker() = default;
  TypeWalker(const TypeWalker &) = default;
  virtual ~TypeWalker() = default;
  
  virtual void anchor();
};

} // end namespace swift

#endif
