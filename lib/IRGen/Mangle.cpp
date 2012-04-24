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
#include "Linking.h"
#include "IRGen.h"

using namespace swift;
using namespace irgen;

/// Translate the given operator character into its mangled form.
///
/// Current operator characters:    /=-+*%<>!&|^~ and the special operator '..'
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
  case '~': return 't'; // 'tilde'
  case '.': return 'z'; // 'period'
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
    void mangleType(Type type, ExplosionKind kind, unsigned uncurryingLevel);

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
  switch (ctx->getContextKind()) {
  case DeclContextKind::BuiltinModule:
    llvm_unreachable("mangling member of builtin module!");

  case DeclContextKind::TranslationUnit: {
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

    // This should work, because the language should be restricting
    // the name of a module to be a valid language identifier.
    mangleIdentifier(module->Name);
    addSubstitution(module);
    return;
  }

  case DeclContextKind::OneOfType: {
    OneOfType *oneof = cast<OneOfType>(ctx);

    if (tryMangleSubstitution(oneof)) return;
    mangleDeclContext(ctx->getParent());
    mangleIdentifier(oneof->getDecl()->getName());
    addSubstitution(oneof);
    return;
  }

  case DeclContextKind::ExtensionDecl:
    // Mandle the extension as the original type.
    mangleType(cast<ExtensionDecl>(ctx)->getExtendedType(),
               ExplosionKind::Minimal, 0);
    return;

  case DeclContextKind::CapturingExpr:
  case DeclContextKind::TopLevelCodeDecl:
    // FIXME: we don't need to agree about these across components, but
    // that's no excuse for not mangling *something* in here.
    break;

  case DeclContextKind::ProtocolType:
    // It's not clear what members would be here that need to be mangled.
    llvm_unreachable("reference to member of protocol?");
  }

  llvm_unreachable("bad decl context");
}

void Mangler::mangleDeclName(NamedDecl *decl) {
  // decl ::= context identifier
  mangleDeclContext(decl->getDeclContext());
  
  // Special case: mangle getters and setters.
  // FIXME: Come up with a more sane mangling.
  if (FuncDecl *Func = dyn_cast<FuncDecl>(decl)) {
    if (Decl *D = Func->getGetterDecl()) {
      if (VarDecl *Var = dyn_cast<VarDecl>(D)) {
        Buffer << "__get";
        mangleIdentifier(Var->getName());
        return;
      }
      
      assert(isa<SubscriptDecl>(D) && "Unknown getter kind?");
      Buffer << "__subset";
      return;
    }

    if (Decl *D = Func->getSetterDecl()) {
      if (VarDecl *Var = dyn_cast<VarDecl>(D)) {
        Buffer << "__set";
        mangleIdentifier(Var->getName());
        return;
      }
      
      assert(isa<SubscriptDecl>(D) && "Unknown setter kind?");
      Buffer << "__subget";
      return;
    }
  }
  
  mangleIdentifier(decl->getName());
}

/// Mangle a type into the buffer.
void Mangler::mangleType(Type type, ExplosionKind explosion,
                         unsigned uncurryLevel) {
  TypeBase *base = type.getPointer();

  switch (base->getKind()) {
  case TypeKind::Error:
    llvm_unreachable("mangling error type");
  case TypeKind::UnstructuredDependent:
    llvm_unreachable("mangling dependent type");

  case TypeKind::MetaType:
    llvm_unreachable("Cannot mangle metatype yet");
  case TypeKind::Module:
    llvm_unreachable("Cannot mangle module type yet");

  // We don't care about these types being a bit verbose because we
  // don't expect them to come up that often in API names.
  case TypeKind::BuiltinFloat:
    switch (cast<BuiltinFloatType>(type)->getFPKind()) {
    case BuiltinFloatType::IEEE16: Buffer << "f16"; return;
    case BuiltinFloatType::IEEE32: Buffer << "f32"; return;
    case BuiltinFloatType::IEEE64: Buffer << "f64"; return;
    case BuiltinFloatType::IEEE80: Buffer << "f80"; return;
    case BuiltinFloatType::IEEE128: Buffer << "f128"; return;
    case BuiltinFloatType::PPC128: llvm_unreachable("ppc128 not supported");
    }
    llvm_unreachable("bad floating-point kind");
  case TypeKind::BuiltinInteger:
    Buffer << "i" << cast<BuiltinIntegerType>(type)->getBitWidth();
    return;
  case TypeKind::BuiltinRawPointer:
    Buffer << "p";
    return;
  case TypeKind::BuiltinObjectPointer:
    Buffer << "o";
    return;

#define SUGARED_TYPE(id, parent) \
  case TypeKind::id: \
    return mangleType(cast<id##Type>(base)->getDesugaredType(), \
                      explosion, uncurryLevel);
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"

  case TypeKind::LValue:
    Buffer << 'R';
    return mangleType(cast<LValueType>(base)->getObjectType(),
                      ExplosionKind::Minimal, 0);

  case TypeKind::Tuple: {
    TupleType *tuple = cast<TupleType>(base);
    // type ::= 'T' tuple-field+ '_'
    // tuple-field ::= identifier? type
    Buffer << 'T';
    for (auto &field : tuple->getFields()) {
      if (field.hasName())
        mangleIdentifier(field.getName());
      mangleType(field.getType(), explosion, 0);
    }
    Buffer << '_';
    return;
  }

  case TypeKind::OneOf: {
    // Try to mangle the entire name as a substitution.
    // type ::= substitution
    if (tryMangleSubstitution(base))
      return;

    // type ::= 'N' decl
    Buffer << 'N';
    mangleDeclName(cast<OneOfType>(base)->getDecl());

    addSubstitution(base);
    return;
  }

  case TypeKind::Function: {
    // type ::= 'F' type type (curried)
    // type ::= 'f' type type (uncurried)
    FunctionType *fn = cast<FunctionType>(base);
    Buffer << (uncurryLevel > 0 ? 'f' : 'F');
    mangleType(fn->getInput(), explosion, 0);
    mangleType(fn->getResult(), explosion,
               (uncurryLevel > 0 ? uncurryLevel - 1 : 0));
    return;
  }

  case TypeKind::Array: {
    // type ::= 'A' integer type
    ArrayType *array = cast<ArrayType>(base);
    Buffer << 'A';
    Buffer << array->getSize();
    mangleType(array->getBaseType(), ExplosionKind::Minimal, 0);
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

void LinkEntity::mangle(raw_ostream &buffer) const {
  // mangled-name ::= '_T' identifier+ type?

  // Add the prefix.
  buffer << "_T"; // T is for Tigger

  Mangler mangler(buffer);

  mangler.mangleDeclName(TheDecl);

  // Mangle in a type as well.  Note that we have to mangle the type
  // on all kinds of declarations, even variables, because at the
  // moment they can *all* be overloaded.
  if (ValueDecl *valueDecl = dyn_cast<ValueDecl>(TheDecl))
    if (!isa<TypeAliasDecl>(TheDecl))
      mangler.mangleType(valueDecl->getType(),
                         getExplosionKind(),
                         getUncurryLevel());

  // Add a getter/setter suffix if applicable.
  switch (getKind()) {
  case Kind::Function: break;
  case Kind::Other: break;
  case Kind::Getter: buffer << "g"; break;
  case Kind::Setter: buffer << "s"; break;
  }

  // TODO: mangle generics information here.
}
