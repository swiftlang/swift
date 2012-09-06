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
#include "llvm/Support/SaveAndRestore.h"
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

static bool isSwiftModule(Module *module) {
  return (!module->getParent() && module->Name.str() == "swift");
}

namespace {
  enum class IncludeType : bool { No, Yes };
  
  struct ArchetypeInfo {
    unsigned Depth;
    unsigned Index;
  };

  /// A helpful little wrapper for a value that should be mangled
  /// in a particular, compressed value.
  class Index {
    unsigned N;
  public:
    explicit Index(unsigned n) : N(n) {}
    friend raw_ostream &operator<<(raw_ostream &out, Index n) {
      if (n.N != 0) out << (n.N - 1);
      return (out << '_');
    }
  };

  /// A class for mangling declarations.
  class Mangler {
    raw_ostream &Buffer;
    llvm::DenseMap<void*, unsigned> Substitutions;
    llvm::DenseMap<ArchetypeType*, ArchetypeInfo> Archetypes;
    unsigned ArchetypesDepth = 0;

  public:
    Mangler(raw_ostream &buffer) : Buffer(buffer) {}
    void mangleDeclContext(DeclContext *ctx);
    void mangleDeclName(ValueDecl *decl, IncludeType includeType);
    void mangleDeclType(ValueDecl *decl, ExplosionKind kind,
                        unsigned uncurryingLevel);
    void mangleType(Type type, ExplosionKind kind, unsigned uncurryingLevel);

  private:
    void mangleFunctionType(AnyFunctionType *fn, ExplosionKind explosionKind,
                            unsigned uncurryingLevel);
    void mangleNominalType(NominalTypeDecl *decl, ExplosionKind explosionKind);
    void mangleProtocolList(ArrayRef<ProtocolDecl*> protocols);
    void mangleProtocolList(ArrayRef<Type> protocols);
    void mangleProtocolName(ProtocolDecl *protocol);
    void mangleIdentifier(Identifier ident);
    void mangleGetterOrSetterContext(FuncDecl *fn);
    void manglePolymorphicType(const GenericParamList *genericParams, Type T,
                               ExplosionKind explosion, unsigned uncurryLevel,
                               bool mangleAsFunction);
    bool tryMangleStandardSubstitution(NominalTypeDecl *type);
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
    if (isSwiftModule(module)) {
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

  case DeclContextKind::NominalTypeDecl:
    mangleNominalType(cast<NominalTypeDecl>(ctx), ExplosionKind::Minimal);
    return;

  case DeclContextKind::ExtensionDecl: {
    // Mangle the extension as the originally-extended type.
    Type type = cast<ExtensionDecl>(ctx)->getExtendedType();
    if (NominalType *nom = type->getAs<NominalType>()) {
      mangleNominalType(nom->getDecl(), ExplosionKind::Minimal);
      return;
    }
    mangleNominalType(type->castTo<UnboundGenericType>()->getDecl(),
                      ExplosionKind::Minimal);
    return;
  }

  case DeclContextKind::CapturingExpr:
    // FIXME: We need a real solution here for local types.
    if (FuncExpr *FE = dyn_cast<FuncExpr>(ctx)) {
      if (FE->getDecl()) {
        if (FE->getDecl()->isGetterOrSetter()) {
          mangleGetterOrSetterContext(FE->getDecl());
          return;
        }
        mangleDeclName(FE->getDecl(), IncludeType::Yes);
        return;
      }
    }
    llvm_unreachable("unnamed closure mangling not yet implemented");

  case DeclContextKind::ConstructorDecl:
    mangleDeclName(cast<ConstructorDecl>(ctx), IncludeType::Yes);
    return;

  case DeclContextKind::DestructorDecl:
    mangleDeclName(cast<DestructorDecl>(ctx), IncludeType::No);
    return;

  case DeclContextKind::TopLevelCodeDecl:
    // FIXME: I'm not sure this is correct.
    return;
  }

  llvm_unreachable("bad decl context");
}

void Mangler::mangleGetterOrSetterContext(FuncDecl *func) {
  assert(func->isGetterOrSetter());
  Decl *D = func->getGetterDecl();
  if (!D) D = func->getSetterDecl();
  assert(D && "no value type for getter/setter!");
  assert(isa<VarDecl>(D) || isa<SubscriptDecl>(D));

  mangleDeclName(cast<ValueDecl>(D), IncludeType::No);

  // We mangle the type with a canonical set of parameters because
  // objects nested within functions are shared across all expansions
  // of the function.
  mangleDeclType(cast<ValueDecl>(D), ExplosionKind::Minimal, /*uncurry*/ 0);

  if (func->getGetterDecl()) {
    Buffer << 'g';
  } else {
    Buffer << 's';
  }
}

void Mangler::manglePolymorphicType(const GenericParamList *genericParams,
                                    Type T, ExplosionKind explosion,
                                    unsigned uncurryLevel,
                                    bool mangleAsFunction) {
  SmallVector<const GenericParamList *, 2> paramLists;
  for (; genericParams; genericParams = genericParams->getOuterParameters())
    paramLists.push_back(genericParams);

  // FIXME: Prefix?
  llvm::SaveAndRestore<unsigned> oldArchetypesDepth(ArchetypesDepth);
  for (auto gp = paramLists.rbegin(), gpEnd = paramLists.rend(); gp != gpEnd;
       ++gp) {
    ArchetypesDepth++;
    unsigned index = 0;
    // FIXME: Only mangle the archetypes and protocol requirements that
    // matter, rather than everything.
    for (auto archetype : (*gp)->getAllArchetypes()) {
      // Remember the current depth and level.
      ArchetypeInfo info;
      info.Depth = ArchetypesDepth;
      info.Index = index++;
      Archetypes.insert(std::make_pair(archetype, info));

      // Mangle this type parameter.
      //   <generic-parameter> ::= <protocol-list> _
      mangleProtocolList(archetype->getConformsTo());
      Buffer << '_';
    }
  }
  Buffer << '_';

  if (mangleAsFunction)
    mangleFunctionType(T->castTo<AnyFunctionType>(), explosion, uncurryLevel);
  else
    mangleType(T, explosion, uncurryLevel);
}

void Mangler::mangleDeclName(ValueDecl *decl, IncludeType includeType) {
  // decl ::= context identifier
  mangleDeclContext(decl->getDeclContext());

  if (isa<ConstructorDecl>(decl)) {
    Buffer << 'C';
  } else {
    mangleIdentifier(decl->getName());
  }

  if (includeType == IncludeType::No) return;

  // We mangle the type with a canonical set of parameters because
  // objects nested within functions are shared across all expansions
  // of the function.
  mangleDeclType(decl, ExplosionKind::Minimal, /*uncurry*/ 0);
}

void Mangler::mangleDeclType(ValueDecl *decl, ExplosionKind explosion,
                             unsigned uncurryLevel) {
  if (isa<SubscriptDecl>(decl)) {
    auto genericParams = decl->getDeclContext()->getGenericParamsOfContext();
    if (genericParams) {
      manglePolymorphicType(genericParams, decl->getType(), explosion,
                            uncurryLevel, /*mangleAsFunction=*/false);
      return;
    }
  }
  if (!isa<TypeDecl>(decl))
    mangleType(decl->getType(), explosion, uncurryLevel);
}

/// Mangle a type into the buffer.
///
/// Type manglings should never start with [0-9_] or end with [0-9].
///
/// <type> ::= A <natural> <type>    # fixed-sized arrays
/// <type> ::= Bf <natural> _        # Builtin.Float
/// <type> ::= Bi <natural> _        # Builtin.Integer
/// <type> ::= BO                    # Builtin.ObjCPointer
/// <type> ::= Bo                    # Builtin.ObjectPointer
/// <type> ::= Bp                    # Builtin.RawPointer
/// <type> ::= C <decl>              # class (substitutable)
/// <type> ::= F <type> <type>       # function type
/// <type> ::= f <type> <type>       # uncurried function type
/// <type> ::= G <type> <type>+ _    # bound generic type
/// <type> ::= O <decl>              # oneof (substitutable)
/// <type> ::= P <protocol-list> _   # protocol composition
/// <type> ::= Q <index>             # archetype with depth=0, index=N
/// <type> ::= Qd <index> <index>    # archetype with depth=M+1, index=N
/// <type> ::= R <type>              # lvalue
/// <type> ::= T <tuple-element>* _  # tuple
/// <type> ::= U <generic-parameter>+ _ <type>
/// <type> ::= V <decl>              # struct (substitutable)
///
/// <index> ::= _                    # 0
/// <index> ::= <natural> _          # N+1
///
/// <tuple-element> ::= <identifier>? <type>
void Mangler::mangleType(Type type, ExplosionKind explosion,
                         unsigned uncurryLevel) {
  TypeBase *base = type.getPointer();

  switch (base->getKind()) {
  case TypeKind::Error:
    llvm_unreachable("mangling error type");
  case TypeKind::UnstructuredUnresolved:
  case TypeKind::DeducibleGenericParam:
  case TypeKind::UnboundGeneric:
    llvm_unreachable("mangling unresolved type");
  case TypeKind::TypeVariable:
    llvm_unreachable("mangling type variable");

  case TypeKind::Module:
    llvm_unreachable("Cannot mangle module type yet");
      
  // We don't care about these types being a bit verbose because we
  // don't expect them to come up that often in API names.
  case TypeKind::BuiltinFloat:
    switch (cast<BuiltinFloatType>(base)->getFPKind()) {
    case BuiltinFloatType::IEEE16: Buffer << "Bf16_"; return;
    case BuiltinFloatType::IEEE32: Buffer << "Bf32_"; return;
    case BuiltinFloatType::IEEE64: Buffer << "Bf64_"; return;
    case BuiltinFloatType::IEEE80: Buffer << "Bf80_"; return;
    case BuiltinFloatType::IEEE128: Buffer << "Bf128_"; return;
    case BuiltinFloatType::PPC128: llvm_unreachable("ppc128 not supported");
    }
    llvm_unreachable("bad floating-point kind");
  case TypeKind::BuiltinInteger:
    Buffer << "Bi" << cast<BuiltinIntegerType>(base)->getBitWidth() << '_';
    return;
  case TypeKind::BuiltinRawPointer:
    Buffer << "Bp";
    return;
  case TypeKind::BuiltinObjectPointer:
    Buffer << "Bo";
    return;
  case TypeKind::BuiltinObjCPointer:
    Buffer << "BO";
    return;

#define SUGARED_TYPE(id, parent) \
  case TypeKind::id: \
    return mangleType(cast<id##Type>(base)->getDesugaredType(), \
                      explosion, uncurryLevel);
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"

  case TypeKind::MetaType:
    Buffer << 'M';
    return mangleType(cast<MetaTypeType>(base)->getInstanceType(),
                      ExplosionKind::Minimal, 0);

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

  case TypeKind::OneOf:
    return mangleNominalType(cast<OneOfType>(base)->getDecl(), explosion);

  case TypeKind::Protocol:
    return mangleNominalType(cast<ProtocolType>(base)->getDecl(), explosion);

  case TypeKind::Struct:
    return mangleNominalType(cast<StructType>(base)->getDecl(), explosion);

  case TypeKind::Class:
    return mangleNominalType(cast<ClassType>(base)->getDecl(), explosion);

  case TypeKind::BoundGeneric: {
    // type ::= 'G' <type> <type>+ '_'
    auto type = cast<BoundGenericType>(base);
    Buffer << 'G';
    mangleNominalType(type->getDecl(), explosion);
    for (auto arg : type->getGenericArgs()) {
      mangleType(arg, ExplosionKind::Minimal, /*uncurry*/ 0);
    }
    Buffer << '_';
    return;
  }

  case TypeKind::PolymorphicFunction: {
    // <type> ::= U <generic-parameter>+ _ <type>
    // 'U' is for "universal qualification".
    // The nested type is always a function type.
    PolymorphicFunctionType *fn = cast<PolymorphicFunctionType>(base);
    Buffer << 'U';
    manglePolymorphicType(&fn->getGenericParams(), fn, explosion, uncurryLevel,
                          /*mangleAsFunction=*/true);
    return;
  }

  case TypeKind::Archetype: {
    // <type> ::= Q <index>             # archetype with depth=0, index=N
    // <type> ::= Qd <index> <index>    # archetype with depth=M+1, index=N

    // Find the archetype information.  It may be possible for this to
    // fail for local declarations --- that might be okay; it means we
    // probably need to insert contexts for all the enclosing contexts.
    // And of course, linkage is not critical for such things.
    auto it = Archetypes.find(cast<ArchetypeType>(base));
    assert(it != Archetypes.end());
    auto &info = it->second;
    assert(ArchetypesDepth >= info.Depth);

    Buffer << 'Q';
    unsigned relativeDepth = ArchetypesDepth - info.Depth;
    if (relativeDepth != 0) {
      Buffer << 'd' << Index(relativeDepth - 1);
    }
    Buffer << Index(info.Index);
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

  case TypeKind::ProtocolComposition: {
    // We mangle ProtocolType and ProtocolCompositionType using the
    // same production:
    //   <type> ::= P <protocol-list> _
    // As a special case, if there is exactly one protocol in the
    // list, and it is a substitution candidate, then the *entire*
    // producton is substituted.

    auto protocols = cast<ProtocolCompositionType>(base)->getProtocols();
    assert(protocols.size() != 1);
    Buffer << 'P';
    mangleProtocolList(protocols);
    Buffer << '_';
    return;
  }
  }
  llvm_unreachable("bad type kind");
}

/// Mangle a list of protocols.  Each protocol is a substitution
/// candidate.
///   <protocol-list> ::= <protocol-name>+
void Mangler::mangleProtocolList(ArrayRef<Type> protocols) {
  for (auto protoTy : protocols) {
    mangleProtocolName(protoTy->castTo<ProtocolType>()->getDecl());
  }
}
void Mangler::mangleProtocolList(ArrayRef<ProtocolDecl*> protocols) {
  for (auto protocol : protocols) {
    mangleProtocolName(protocol);
  }
}

/// Mangle the name of a protocol as a substitution candidate.
void Mangler::mangleProtocolName(ProtocolDecl *protocol) {
  //  <protocol-name> ::= <decl>      # substitutable
  // The <decl> in a protocol-name is the same substitution
  // candidate as a protocol <type>, but it is mangled without
  // the surrounding 'P'...'_'.
  ProtocolType *type = cast<ProtocolType>(protocol->getDeclaredType());
  if (tryMangleSubstitution(type))
    return;
  mangleDeclName(protocol, IncludeType::No);
  addSubstitution(type);
}

static char getSpecifierForNominalType(NominalTypeDecl *decl) {
  switch (decl->getKind()) {
#define NOMINAL_TYPE_DECL(id, parent)
#define DECL(id, parent) \
  case DeclKind::id:
#include "swift/AST/DeclNodes.def"
    llvm_unreachable("not a nominal type");

  case DeclKind::Protocol: return 'P';
  case DeclKind::Class: return 'C';
  case DeclKind::OneOf: return 'O';
  case DeclKind::Struct: return 'V';
  }
  llvm_unreachable("bad decl kind");
}

void Mangler::mangleNominalType(NominalTypeDecl *decl,
                                ExplosionKind explosion) {
  // Check for certain standard types.
  if (tryMangleStandardSubstitution(decl))
    return;

  // For generic types, this uses the unbound type.
  TypeBase *key = decl->getDeclaredType().getPointer();

  // Try to mangle the entire name as a substitution.
  // type ::= substitution
  if (tryMangleSubstitution(key))
    return;

  Buffer << getSpecifierForNominalType(decl);
  mangleDeclName(decl, IncludeType::No);

  addSubstitution(key);
}

bool Mangler::tryMangleStandardSubstitution(NominalTypeDecl *decl) {
  // Bail out if our parent isn't the swift standard library.
  Module *parent = dyn_cast<Module>(decl->getDeclContext());
  if (!parent || !isSwiftModule(parent)) return false;

  // Standard substitutions shouldn't start with 's' (because that's
  // reserved for the swift module itself) or a digit or '_'.

  StringRef name = decl->getName().str();
  if (name == "Int64") {
    Buffer << "Si";
    return true;
  } else if (name == "UInt64") {
    Buffer << "Su";
    return true;
  } else if (name == "Bool") {
    Buffer << "Sb";
    return true;
  } else if (name == "Char") {
    Buffer << "Sc";
    return true;
  } else if (name == "Double") {
    Buffer << "Sd";
    return true;
  } else if (name == "Float") {
    Buffer << "Sf";
    return true;
  } else if (name == "String") {
    Buffer << "SS";
    return true;
  } else {
    return false;
  }
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

  case ValueWitness::Size:
  case ValueWitness::Alignment:
  case ValueWitness::Stride:
    llvm_unreachable("not a function witness");
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
  switch (getKind()) {
  case Kind::ClassMetadata:
  case Kind::Destructor:
    mangler.mangleDeclContext(cast<ClassDecl>(getDecl()));
    break;

  default:
    mangler.mangleDeclName(getDecl(), IncludeType::No);
    break;
  }

  // Mangle in a type as well.  Note that we have to mangle the type
  // on all kinds of declarations, even variables, because at the
  // moment they can *all* be overloaded.
  if (ValueDecl *valueDecl = dyn_cast<ValueDecl>(getDecl()))
    mangler.mangleDeclType(valueDecl, getExplosionKind(), getUncurryLevel());

  // Add a suffix if applicable.
  switch (getKind()) {
  case Kind::Function: break;
  case Kind::Other: break;
  case Kind::Getter: buffer << "g"; break;
  case Kind::Setter: buffer << "s"; break;
  case Kind::Destructor: buffer << "D"; break; // for 'destructor'
  case Kind::ClassMetadata: buffer << 'H'; break; // for 'heap'
  case Kind::ValueWitness: llvm_unreachable("filtered out!");
  }
}
