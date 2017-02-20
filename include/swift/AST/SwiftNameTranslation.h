//===--- SwiftNameTranslation.h - Swift Name Translation --------*- C++ -*-===//
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

#ifndef SWIFT_NAME_TRANSLATION_H
#define SWIFT_NAME_TRANSLATION_H

#include "swift/AST/Identifier.h"

namespace swift {
  class ValueDecl;
  class EnumElementDecl;

namespace objc_translation {
  enum CustomNamesOnly_t : bool {
    Normal = false,
    CustomNamesOnly = true,
  };

  StringRef getNameForObjC(const ValueDecl *VD,
                           CustomNamesOnly_t customNamesOnly = Normal);

  /// Print the ObjC name of an enum element decl to OS, also allowing the client
  /// to specify a preferred name other than the decl's original name.
  ///
  /// Returns true if the decl has a custom ObjC name (@objc); false otherwise.
  bool printSwiftEnumElemNameInObjC(const EnumElementDecl *EL,
                                    llvm::raw_ostream &OS,
                                    Identifier PreferredName = Identifier());

  /// Get the name for a value decl D if D is exported to ObjC, PreferredName is
  /// specified to perform what-if analysis, shadowing D's original name during
  /// computation.
  ///
  /// Returns a pair of Identifier and ObjCSelector, only one of which is valid.
  std::pair<Identifier, ObjCSelector>
  getObjCNameForSwiftDecl(const ValueDecl *VD, DeclName PreferredName = DeclName());

} // end namespace objc_translation
} // end namespace swift

#endif
