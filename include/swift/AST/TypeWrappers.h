//===--------- TypeWrappers.h - Type Wrapper ASTs ---------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines helper types for type wrappers.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_TYPE_WRAPPERS_H
#define SWIFT_AST_TYPE_WRAPPERS_H

namespace swift {

struct TypeWrapperInfo {
  CustomAttr *Attr;
  NominalTypeDecl *Wrapper;
  NominalTypeDecl *AttachedTo;
  bool IsInferred;

  TypeWrapperInfo(CustomAttr *attr, NominalTypeDecl *wrapperDecl,
                  NominalTypeDecl *attachedTo, bool isInferred)
      : Attr(attr), Wrapper(wrapperDecl), AttachedTo(attachedTo),
        IsInferred(isInferred) {}

  TypeWrapperInfo asInferred() const {
    return {Attr, Wrapper, AttachedTo, true};
  }
};

} // end namespace swift

#endif // SWIFT_AST_TYPE_WRAPPERS_H
