//===--- Mangle.cpp - Swift Name Mangling --------------------------------===//
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
//  This file implements declaration name mangling in Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ErrorHandling.h"

#include "IRGenModule.h"

using namespace swift;
using namespace irgen;

/// Translate the given operator character into its mangled form.
///
/// Current operator characters:    /=-+*%<>!&|^
static char mangleOperatorChar(char op) {
  switch (op) {
  case '&': return 'a'; // 'and'
  case '/': return 'd'; // 'divide'
  case '=': return 'e'; // 'equal'
  case '>': return 'g'; // 'greater'
  case '<': return 'l'; // 'less'
  case '*': return 'm'; // 'multiply'
  case '!': return 'n'; // 'negate'
  case '|': return 'o'; // 'or'
  case '+': return 'p'; // 'plus'
  case '%': return 'r'; // 'remainder'
  case '-': return 's'; // 'subtract'
  case '^': return 'x'; // 'xor'
  default: llvm_unreachable("bad identifier character");
  }
}

/// Mangle an identifier into the buffer.
static void mangleIdentifier(raw_ostream &buffer, Identifier ident) {
  StringRef str = ident.str();
  assert(!str.empty() && "mangling an empty identifier!");

  // Mangle normal identifiers as
  //   count identifier-char+
  // where the count is the number of characters in the identifier,
  // and where individual identifier characters represent themselves.
  if (!ident.isOperator()) {
    buffer << str.size() << str;
    return;
  }

  // Mangle operator identifiers as
  //   'op' count operator-char+
  // where the count is the number of characters in the operator,
  // and where the individual operator characters are translated.
  buffer << "op";

  buffer << str.size();
  for (unsigned i = 0, e = str.size(); i != e; ++i) {
    buffer << mangleOperatorChar(str[i]);
  }
}

static bool shouldMangle(NamedDecl *D) {
  // Everything not declared in global context needs to be mangled.
  if (!isa<TranslationUnitDecl>(D->Context)) return true;

  // Don't mangle a function named main.
  if (isa<FuncDecl>(D) && D->Name.str() == "main")
    return false;

  return true;
}

void IRGenModule::mangle(raw_ostream &buffer, NamedDecl *D) {
  // Check for declarations which should not be mangled.
  if (!shouldMangle(D)) {
    buffer << D->Name.str();
    return;
  }

  // Otherwise, add the prefix.
  buffer << "_T"; // T is for Tigger

  // TODO: mangle enclosing contexts here.

  mangleIdentifier(buffer, D->Name);

  // TODO: mangle overload and generics information here.
}
