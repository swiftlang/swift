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

#include "swift/AST/Attr.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ErrorHandling.h"

#include "IRGen.h"
#include "IRGenModule.h"
#include "Linking.h"
#include "ValueWitness.h"

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
  enum class IncludeType : bool { No, Yes };

  /// A class for mangling declarations.
  class Mangler {
    raw_ostream &Buffer;
    llvm::DenseMap<void*, unsigned> Substitutions;

  public:
    Mangler(raw_ostream &buffer) : Buffer(buffer) {}
    void mangleDeclName(ValueDecl *decl, IncludeType includeType);
    void mangleType(Type type, ExplosionKind kind, unsigned uncurryingLevel);

  private:
    void mangleFunctionType(AnyFunctionType *fn, ExplosionKind explosionKind,
                            unsigned uncurryingLevel);
    void mangleDeclContext(DeclContext *ctx);
    void mangleIdentifier(Identifier ident);
    void mangleGetterOrSetterType(FuncDecl *fn);
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

  case DeclContextKind::NominalTypeDecl: {
    if (isa<OneOfDecl>(ctx)) {
      OneOfDecl *oneof = cast<OneOfDecl>(ctx);

      // FIXME: The mangling rule here is kind of weird.
      if (tryMangleSubstitution(oneof->getDeclaredType())) return;
      mangleDeclContext(ctx->getParent());
      mangleIdentifier(oneof->getName());
      addSubstitution(oneof->getDeclaredType());
      return;
    }
    if (isa<StructDecl>(ctx) || isa<ClassDecl>(ctx)) {
      // FIXME: The mangling rule here is kind of weird.
      mangleType(cast<NominalTypeDecl>(ctx)->getDeclaredType(),
                 ExplosionKind::Minimal, 0);
      return;
    }
    // It's not clear what would be mangled for protocols.
    llvm_unreachable("Unexpected context");
  }

  case DeclContextKind::ExtensionDecl:
    // Mandle the extension as the original type.
    mangleType(cast<ExtensionDecl>(ctx)->getExtendedType(),
               ExplosionKind::Minimal, 0);
    return;

  case DeclContextKind::CapturingExpr:
    // FIXME: We need a real solution here for local types.
    if (FuncExpr *FE = dyn_cast<FuncExpr>(ctx)) {
      if (FE->getDecl()) {
        mangleDeclName(FE->getDecl(), IncludeType::Yes);
        return;
      }
    }
    llvm_unreachable("unnamed closure mangling not yet implemented");

  case DeclContextKind::ConstructorDecl:
    mangleDeclName(cast<ConstructorDecl>(ctx), IncludeType::Yes);
    return;

  case DeclContextKind::TopLevelCodeDecl:
    // FIXME: I'm not sure this is correct.
    return;
  }

  llvm_unreachable("bad decl context");
}

void Mangler::mangleGetterOrSetterType(FuncDecl *func) {
  assert(func->isGetterOrSetter());
  Decl *D = func->getGetterDecl();
  if (!D) D = func->getSetterDecl();
  assert(D && "no value type for getter/setter!");

  // We mangle the type with a canonical set of parameters because
  // objects nested within functions are shared across all expansions
  // of the function.
  assert(isa<VarDecl>(D) || isa<SubscriptDecl>(D));
  mangleType(cast<ValueDecl>(D)->getType(),
             ExplosionKind::Minimal,
             /*uncurry*/ 0);

  if (func->getGetterDecl()) {
    Buffer << 'g';
  } else {
    Buffer << 's';
  }
}

void Mangler::mangleDeclName(ValueDecl *decl, IncludeType includeType) {
  // decl ::= context identifier
  mangleDeclContext(decl->getDeclContext());

  mangleIdentifier(decl->getName());

  if (includeType == IncludeType::No) return;

  // Special case for getters and setters.
  if (auto func = dyn_cast<FuncDecl>(decl))
    if (func->isGetterOrSetter())
      return mangleGetterOrSetterType(func);

  // We mangle the type with a canonical set of parameters because
  // objects nested within functions are shared across all expansions
  // of the function.
  mangleType(decl->getType(), ExplosionKind::Minimal, /*uncurry*/ 0);
}

/// Mangle a type into the buffer.
///
/// Type manglings should never start with [0-9_].
///
/// <type> ::= A <natural> <type>    # fixed-sized arrays
/// <type> ::= C <type>* _           # protocol composition (substitutable)
/// <type> ::= F <type> <type>       # function type
/// <type> ::= f <type> <type>       # uncurried function type
/// <type> ::= f <natural>           # Builtin.Float
/// <type> ::= i <natural>           # Builtin.Integer
/// <type> ::= N <decl>              # oneof, struct, or class (substitutable)
/// <type> ::= O                     # Builtin.ObjCPointer
/// <type> ::= o                     # Builtin.ObjectPointer
/// <type> ::= P <decl>              # protocol (substitutable)
/// <type> ::= p                     # Builtin.RawPointer
/// <type> ::= R <type>              # lvalue
/// <type> ::= T <tuple-element>* _  # tuple
///
/// <tuple-element> ::= <identifier>? <type>
void Mangler::mangleType(Type type, ExplosionKind explosion,
                         unsigned uncurryLevel) {
  TypeBase *base = type.getPointer();

  switch (base->getKind()) {
  case TypeKind::Error:
    llvm_unreachable("mangling error type");
  case TypeKind::UnstructuredUnresolved:
    llvm_unreachable("mangling unresolved type");

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
  case TypeKind::BuiltinObjCPointer:
    Buffer << "O";
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
    mangleDeclName(cast<OneOfType>(base)->getDecl(), IncludeType::No);

    addSubstitution(base);
    return;
  }

  case TypeKind::Struct: {
    // Try to mangle the entire name as a substitution.
    // type ::= substitution
    if (tryMangleSubstitution(base))
      return;

    // FIXME: Leaving this the same as oneof for the moment, to avoid changing
    // the mangling, but this should probably change eventually.
    // type ::= 'N' decl
    Buffer << 'N';
    mangleDeclName(cast<StructType>(base)->getDecl(), IncludeType::No);

    addSubstitution(base);
    return;
  }

  case TypeKind::Class: {
    // Try to mangle the entire name as a substitution.
    // type ::= substitution
    if (tryMangleSubstitution(base))
      return;

    // FIXME: Leaving this the same as oneof for the moment, to avoid changing
    // the mangling, but this should probably change eventually.
    // type ::= 'N' decl
    Buffer << 'N';
    mangleDeclName(cast<ClassType>(base)->getDecl(), IncludeType::No);

    addSubstitution(base);
    return;
  }

  case TypeKind::Archetype:
    llvm_unreachable("Cannot mangle archetype yet");

  case TypeKind::PolymorphicFunction: {
    // type ::= 'U' generic-parameter-list type
    // 'U' is for "universal qualification".
    // The nested type is always a function type.
    PolymorphicFunctionType *fn = cast<PolymorphicFunctionType>(base);
    Buffer << 'U';

    for (auto &param : fn->getGenericParams()) {
      (void) param;
      // TODO: bind the archetypes here.
      // TODO: mangle the parameter somehow.
    }
    mangleFunctionType(cast<FunctionType>(base), explosion, uncurryLevel);
    // TODO: unbind the archetypes
    return;
  }

  case TypeKind::Function:
    mangleFunctionType(cast<FunctionType>(base), explosion, uncurryLevel);
    return;

  case TypeKind::Array: {
    // type ::= 'A' integer type
    ArrayType *array = cast<ArrayType>(base);
    Buffer << 'A';
    Buffer << array->getSize();
    mangleType(array->getBaseType(), ExplosionKind::Minimal, 0);
    return;
  };

  case TypeKind::Protocol: {
    if (tryMangleSubstitution(base))
      return;

    // type ::= 'P'
    Buffer << 'P';
    mangleDeclName(cast<ProtocolType>(base)->getDecl(), IncludeType::No);

    addSubstitution(base);
    return;
  }

  case TypeKind::ProtocolComposition: {
    if (tryMangleSubstitution(base))
      return;
    
    // <type> ::= C <type>* _
    Buffer << 'C';
    for (auto Proto : cast<ProtocolCompositionType>(base)->getProtocols())
      mangleType(Proto, ExplosionKind::Minimal, 0);
    Buffer << '_';
    addSubstitution(base);
    return;
  }
  }
  llvm_unreachable("bad type kind");
}

void Mangler::mangleFunctionType(AnyFunctionType *fn,
                                 ExplosionKind explosion,
                                 unsigned uncurryLevel) {
  // type ::= 'F' type type (curried)
  // type ::= 'f' type type (uncurried)
  Buffer << (uncurryLevel > 0 ? 'f' : 'F');
  mangleType(fn->getInput(), explosion, 0);
  mangleType(fn->getResult(), explosion,
             (uncurryLevel > 0 ? uncurryLevel - 1 : 0));
}

static StringRef mangleValueWitness(ValueWitness witness) {
  // The ones with at least one capital are the composite ops, and the
  // capitals correspond roughly to the positions of buffers (as
  // opposed to objects) in the arguments.  That doesn't serve any
  // direct purpose, but it's neat.
  switch (witness) {
  case ValueWitness::AllocateBuffer: return "al";
  case ValueWitness::AssignWithCopy: return "ac";
  case ValueWitness::AssignWithTake: return "at";
  case ValueWitness::DeallocateBuffer: return "de";
  case ValueWitness::Destroy: return "xx";
  case ValueWitness::DestroyBuffer: return "XX";
  case ValueWitness::InitializeBufferWithCopyOfBuffer: return "CP";
  case ValueWitness::InitializeBufferWithCopy: return "Cp";
  case ValueWitness::InitializeWithCopy: return "cp";
  case ValueWitness::InitializeBufferWithTake: return "Tk";
  case ValueWitness::InitializeWithTake: return "tk";
  case ValueWitness::ProjectBuffer: return "pr";
  case ValueWitness::SizeAndAlignment: return "sa";
  }
  llvm_unreachable("bad witness kind");
}

void LinkEntity::mangle(raw_ostream &buffer) const {
  // mangled-name ::= '_Tw' witness-kind type
  if (getKind() == Kind::ValueWitness) {
    buffer << "_Tw";
    buffer << mangleValueWitness(getValueWitness());

    Mangler mangler(buffer);
    mangler.mangleType(getType(), ExplosionKind::Minimal, 0);
    return;
  }

  // Declarations with asm names just use that.
  assert(isDeclKind(getKind()));
  if (!getDecl()->getAttrs().AsmName.empty()) {
    buffer << getDecl()->getAttrs().AsmName;
    return;
  }

  // mangled-name ::= '_T' identifier+ type?

  // Add the prefix.
  buffer << "_T"; // T is for Tigger

  if (isLocalLinkage())
    buffer << "L";

  Mangler mangler(buffer);
  mangler.mangleDeclName(getDecl(), IncludeType::No);

  // Mangle in a type as well.  Note that we have to mangle the type
  // on all kinds of declarations, even variables, because at the
  // moment they can *all* be overloaded.
  if (ValueDecl *valueDecl = dyn_cast<ValueDecl>(getDecl()))
    if (!isa<TypeDecl>(getDecl()))
      mangler.mangleType(valueDecl->getType(),
                         getExplosionKind(),
                         getUncurryLevel());

  // Add a getter/setter suffix if applicable.
  switch (getKind()) {
  case Kind::Function: break;
  case Kind::Other: break;
  case Kind::Getter: buffer << "g"; break;
  case Kind::Setter: buffer << "s"; break;
  case Kind::ValueWitness: llvm_unreachable("filtered out!");
  }

  // TODO: mangle generics information here.
  // Unless this should be in mangleDeclName?
}
