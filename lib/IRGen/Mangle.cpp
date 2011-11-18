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
#include "swift/AST/Module.h"
#include "llvm/ADT/DenseMap.h"
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

namespace {
  /// A class for mangling declarations.
  class Mangler {
    raw_ostream &Buffer;
    llvm::DenseMap<void*, unsigned> Substitutions;

  public:
    Mangler(raw_ostream &buffer) : Buffer(buffer) {}
    void mangleDeclName(NamedDecl *decl);
    void mangleType(Type type);

  private:
    void mangleDeclContext(DeclContext *ctx);
    void mangleIdentifier(Identifier ident);
    bool tryMangleSubstitution(void *ptr);
    void addSubstitution(void *ptr);
  };
}

/// Mangle an identifier into the buffer.
void Mangler::mangleIdentifier(Identifier ident) {
  StringRef str = ident.str();
  assert(!str.empty() && "mangling an empty identifier!");

  // Mangle normal identifiers as
  //   count identifier-char+
  // where the count is the number of characters in the identifier,
  // and where individual identifier characters represent themselves.
  if (!ident.isOperator()) {
    Buffer << str.size() << str;
    return;
  }

  // Mangle operator identifiers as
  //   'op' count operator-char+
  // where the count is the number of characters in the operator,
  // and where the individual operator characters are translated.
  Buffer << "op";

  Buffer << str.size();
  for (unsigned i = 0, e = str.size(); i != e; ++i) {
    Buffer << mangleOperatorChar(str[i]);
  }
}

bool Mangler::tryMangleSubstitution(void *ptr) {
  auto ir = Substitutions.find(ptr);
  if (ir == Substitutions.end()) return false;

  // substitution ::= 'S' integer? '_'

  unsigned index = ir->second;
  Buffer << 'S';
  if (index) Buffer << (index - 1);
  Buffer << '_';
  return true;
}

void Mangler::addSubstitution(void *ptr) {
  Substitutions.insert(std::make_pair(ptr, Substitutions.size()));
}

void Mangler::mangleDeclContext(DeclContext *ctx) {
  assert(isa<Module>(ctx) && "can't yet mangle non-module contexts!");
  Module *module = cast<Module>(ctx);

  // Try the special 'swift' substitution.
  // context ::= Ss
  if (!module->getParent() && module->Name.str() == "swift") {
    Buffer << "Ss";
    return;
  }

  // context ::= substitution identifier*
  // context ::= identifier+

  if (tryMangleSubstitution(module)) return;

  if (DeclContext *parent = module->getParent())
    mangleDeclContext(parent);

  mangleIdentifier(module->Name);
  addSubstitution(module);
}

void Mangler::mangleDeclName(NamedDecl *decl) {
  // decl ::= context identifier
  mangleDeclContext(decl->getDeclContext());
  mangleIdentifier(decl->getName());
}

/// Mangle a type into the buffer.
void Mangler::mangleType(Type type) {
  TypeBase *base = type.getPointer();

  switch (base->Kind) {
  case TypeKind::Error:
    llvm_unreachable("mangling error type");
  case TypeKind::Dependent:
    llvm_unreachable("mangling dependent type");

  // We don't care about these types being a bit verbose because we
  // don't expect them to come up that often in API names.
  case TypeKind::BuiltinFloat32:
    Buffer << "f32"; return;
  case TypeKind::BuiltinFloat64:
    Buffer << "f64"; return;
  case TypeKind::BuiltinInt1:
    Buffer << "i1"; return;
  case TypeKind::BuiltinInt8:
    Buffer << "i8"; return;
  case TypeKind::BuiltinInt16:
    Buffer << "i16"; return;
  case TypeKind::BuiltinInt32:
    Buffer << "i32"; return;
  case TypeKind::BuiltinInt64:
    Buffer << "i64"; return;

  case TypeKind::NameAlias: {
    // TODO: convince Chris that anonymous nominal types are silly.
    TypeAliasDecl *alias = cast<NameAliasType>(base)->TheDecl;

    // Mangle a direct alias of a oneof type using the alias.
    if (isa<OneOfType>(alias->UnderlyingTy.getPointer())) {
      // Try to mangle the entire name as a substitution.
      // type ::= substitution
      if (tryMangleSubstitution(alias))
        return;

      // type ::= 'N' decl
      Buffer << 'N';
      mangleDeclName(alias);

      addSubstitution(alias);

    // Otherwise, mangle the type as its underlying type.
    } else {
      mangleType(alias->UnderlyingTy);
    }
    return;
  }

  case TypeKind::Tuple: {
    TupleType *tuple = cast<TupleType>(base);
    // Look through simple parentheses.
    if (tuple->Fields.size() == 1 && tuple->Fields[0].Name.empty())
      return mangleType(tuple->Fields[0].Ty);

    // type ::= 'T' tuple-field+ '_'
    // tuple-field ::= identifier? type
    Buffer << 'T';
    for (auto &field : tuple->Fields) {
      if (!field.Name.empty())
        mangleIdentifier(field.Name);
      mangleType(field.Ty);
    }
    Buffer << '_';
    return;
  }

  case TypeKind::OneOf: {
    // FIXME: what to do with an anonymous oneof type in a global decl?
    Buffer << 'O';
    return;
  }

  case TypeKind::Function: {
    // type ::= 'F' type type
    FunctionType *fn = cast<FunctionType>(base);
    Buffer << 'F';
    mangleType(fn->Input);
    mangleType(fn->Result);
    return;
  }

  case TypeKind::Array: {
    // type ::= 'A' integer type
    ArrayType *array = cast<ArrayType>(base);
    Buffer << 'A';
    Buffer << array->Size;
    mangleType(array->Base);
    return;
  };

  case TypeKind::Protocol: {
    // FIXME: mangle protocol elements?
    // type ::= 'P'
    Buffer << 'P';
    return;
  }

  }
  llvm_unreachable("bad type kind");
}

static bool shouldMangle(NamedDecl *D) {
  // Everything not declared in global context needs to be mangled.
  if (!isa<TranslationUnit>(D->getDeclContext())) return true;

  // Don't mangle a function named main.
  if (isa<FuncDecl>(D) && D->getName().str() == "main")
    return false;

  return true;
}

void IRGenModule::mangle(raw_ostream &buffer, NamedDecl *decl) {
  // Check for declarations which should not be mangled.
  if (!shouldMangle(decl)) {
    buffer << decl->getName().str();
    return;
  }

  // mangled-name ::= '_T' identifier+ type?

  // Otherwise, add the prefix.
  buffer << "_T"; // T is for Tigger

  Mangler mangler(buffer);

  mangler.mangleDeclName(decl);

  // Mangle in a type as well.  Note that we have to mangle the type
  // on all kinds of declarations, even variables, because at the
  // moment they can *all* be overloaded.
  if (ValueDecl *valueDecl = dyn_cast<ValueDecl>(decl))
    mangler.mangleType(valueDecl->Ty);

  // TODO: mangle generics information here.
}
