//===--- MacroDeclaration.h - Swift Macro Declaration -----------*- C++ -*-===//
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
//
// Data structures that configure the declaration of a macro.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_MACRO_DECLARATION_H
#define SWIFT_AST_MACRO_DECLARATION_H

#include "swift/AST/Identifier.h"

namespace swift {

/// The context in which a macro can be used, which determines the syntax it
/// uses.
enum class MacroContext: uint8_t {
  /// An expression macro, referenced explicitly via "#stringify" or similar
  /// in the source code.
  Expression = 0x01,
  /// A freestanding declaration macro.
  FreestandingDeclaration = 0x02,
  /// An attached declaration macro.
  AttachedDeclaration = 0x03,
};

/// The contexts in which a particular macro declaration can be used.
using MacroContexts = OptionSet<MacroContext>;

enum class MacroIntroducedDeclNameKind {
  Named,
  Overloaded,
  Accessors,
  Prefixed,
  Suffixed,
  Arbitrary,
};

class MacroIntroducedDeclName {
public:
  using Kind = MacroIntroducedDeclNameKind;

private:
  Kind kind;
  Identifier identifier;

public:
  MacroIntroducedDeclName(Kind kind, Identifier identifier = Identifier())
      : kind(kind), identifier(identifier) {};

  static MacroIntroducedDeclName getNamed(Identifier name) {
    return MacroIntroducedDeclName(Kind::Named, name);
  }

  static MacroIntroducedDeclName getOverloaded() {
    return MacroIntroducedDeclName(Kind::Overloaded);
  }

  static MacroIntroducedDeclName getAccessors() {
    return MacroIntroducedDeclName(Kind::Accessors);
  }

  static MacroIntroducedDeclName getPrefixed(Identifier prefix) {
    return MacroIntroducedDeclName(Kind::Prefixed, prefix);
  }

  static MacroIntroducedDeclName getSuffixed(Identifier suffix) {
    return MacroIntroducedDeclName(Kind::Suffixed, suffix);
  }

  static MacroIntroducedDeclName getArbitrary() {
    return MacroIntroducedDeclName(Kind::Arbitrary);
  }

  Kind getKind() const { return kind; }
  Identifier getIdentifier() const { return identifier; }
};

}

#endif // SWIFT_AST_MACRO_DECLARATION_H
