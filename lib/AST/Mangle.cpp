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

#include "swift/AST/Mangle.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/Punycode.h"
#include "swift/ClangImporter/ClangModule.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace Mangle;

/// Translate the given operator character into its mangled form.
///
/// Current operator characters:   @/=-+*%<>!&|^~ and the special operator '..'
static char mangleOperatorChar(char op) {
  switch (op) {
  case '&': return 'a'; // 'and'
  case '@': return 'c'; // 'commercial at sign'
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
  case '~': return 't'; // 'tilde'
  case '^': return 'x'; // 'xor'
  case '.': return 'z'; // 'zperiod' (the z is silent)
  default:
    return op;
  }
}

namespace {
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
}
        
static bool isNonAscii(StringRef str) {
  for (unsigned char c : str) {
    if (c >= 0x80)
      return true;
  }
  return false;
}

/// Mangle an identifier into the buffer.
void Mangler::mangleIdentifier(Identifier ident, OperatorFixity fixity) {
  StringRef str = ident.str();
  assert(!str.empty() && "mangling an empty identifier!");

  // If the identifier contains non-ASCII character, we mangle with an initial
  // X and Punycode the identifier string.
  llvm::SmallString<32> punycodeBuf;

  if (isNonAscii(str)) {
    Buffer << 'X';
    Punycode::encodePunycode(str, punycodeBuf);
    str = punycodeBuf;
  }
  
  // Mangle normal identifiers as
  //   count identifier-char+
  // where the count is the number of characters in the identifier,
  // and where individual identifier characters represent themselves.
  if (!ident.isOperator()) {
    Buffer << str.size() << str;
    return;
  }

  // Mangle operator identifiers as
  //   operator ::= 'o' operator-fixity count operator-char+
  //   operator-fixity ::= 'p' // prefix
  //   operator-fixity ::= 'P' // postfix
  //   operator-fixity ::= 'i' // infix
  // where the count is the number of characters in the operator,
  // and where the individual operator characters are translated.
  Buffer << 'o';
  switch (fixity) {
  case OperatorFixity::NotOperator:
    llvm_unreachable("operator mangled without fixity specified!");
  case OperatorFixity::Infix:
    Buffer << 'i';
    break;
  case OperatorFixity::Prefix:
    Buffer << 'p';
    break;
  case OperatorFixity::Postfix:
    Buffer << 'P';
    break;
  }

  // Mangle ASCII operators directly.
  Buffer << str.size();
  for (char c : str) {
    Buffer << mangleOperatorChar(c);
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

/// Mangle the context of the given declaration as a <context.
/// This is the top-level entrypoint for mangling <context>.
void Mangler::mangleContextOf(ValueDecl *decl) {
  auto clangDecl = decl->getClangDecl();

  // Classes and protocols published to Objective-C have a special context
  // mangling.
  //   known-context ::= 'So'
  if (isa<ClassDecl>(decl) && (clangDecl || decl->isObjC())) {
    assert(!clangDecl || isa<clang::ObjCInterfaceDecl>(clangDecl));
    Buffer << "So";
    return;
  }
  
  if (isa<ProtocolDecl>(decl) && (clangDecl || decl->isObjC())) {
    assert(!clangDecl || isa<clang::ObjCProtocolDecl>(clangDecl));
    Buffer << "So";
    return; 
  }

  // Declarations provided by a C module have a special context mangling.
  //   known-context ::= 'SC'
  if (isa<ClangModule>(decl->getDeclContext())) {
    Buffer << "SC";
    return;
  }

  // Otherwise, just mangle the decl's DC.
  mangleDeclContext(decl->getDeclContext());
}

void Mangler::mangleDeclContext(DeclContext *ctx) {
  switch (ctx->getContextKind()) {
  case DeclContextKind::Module: {
    Module *module = cast<Module>(ctx);

    assert(!isa<BuiltinModule>(module) && "mangling member of builtin module!");

    // Try the special 'swift' substitution.
    // context ::= 'Ss'
    if (module->isStdlibModule()) {
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

  case DeclContextKind::SourceFile:
    mangleDeclContext(ctx->getParent());
    return;

  case DeclContextKind::NominalTypeDecl:
    mangleNominalType(cast<NominalTypeDecl>(ctx), ExplosionKind::Minimal);
    return;

  case DeclContextKind::ExtensionDecl: {
    auto ExtD = cast<ExtensionDecl>(ctx);
    auto ExtTy = ExtD->getExtendedType();
    // Recover from erroneous extension.
    if (ExtTy->is<ErrorType>())
      return mangleDeclContext(ExtD->getDeclContext());

    auto decl = ExtTy->getAnyNominal();
    assert(decl && "extension of non-nominal type?");
    mangleNominalType(decl, ExplosionKind::Minimal);
    return;
  }

  case DeclContextKind::AbstractClosureExpr:
    llvm_unreachable("unnamed closure mangling not yet implemented");

  case DeclContextKind::AbstractFunctionDecl: {
    auto *AFD = cast<AbstractFunctionDecl>(ctx);

    if (auto *FD = dyn_cast<FuncDecl>(AFD)) {
      // FIXME: We need a real solution here for local types.
      if (FD->isGetterOrSetter()) {
        mangleGetterOrSetterContext(FD);
        return;
      }
      mangleDeclName(FD, IncludeType::Yes);
      return;
    }

    if (auto *CD = dyn_cast<ConstructorDecl>(AFD)) {
      mangleDeclName(CD, IncludeType::Yes);
      return;
    }

    mangleDeclName(cast<DestructorDecl>(AFD), IncludeType::No);
    return;
  }

  case DeclContextKind::TopLevelCodeDecl:
    // Mangle the containing module context.
    return mangleDeclContext(ctx->getParent());
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

/// Bind the generic parameters from the given list and its parents.
///
/// \param mangle if true, also emit the mangling for a 'generics'
void Mangler::bindGenericParameters(const GenericParamList *genericParams,
                                    bool mangle = false) {
  assert(genericParams);
  SmallVector<const GenericParamList *, 2> paramLists;
  
  // Determine the depth our parameter list is at. We don't actually need to
  // emit the outer parameters because they should have been emitted as part of
  // the outer context.
  ArchetypesDepth = genericParams->getDepth() + 1;
  unsigned index = 0;
  for (auto archetype : genericParams->getPrimaryArchetypes()) {
    // Remember the current depth and level.
    ArchetypeInfo info;
    info.Depth = ArchetypesDepth;
    info.Index = index++;
    // When mangling for DWARF we will visit the same generic
    // parameter a second time wile mangling its declcontext.
    assert(MangleArchetypesWithContext || !Archetypes.count(archetype));
    Archetypes.insert(std::make_pair(archetype, info));

    if (!mangle) continue;

    // Mangle this type parameter.
    //   <generic-parameter> ::= <protocol-list> _
    // FIXME: Only mangle the archetypes and protocol requirements
    // that matter, rather than everything.
    mangleProtocolList(archetype->getConformsTo());
    Buffer << '_';
  }

  if (mangle) Buffer << '_';  
}

void Mangler::manglePolymorphicType(const GenericParamList *genericParams,
                                    CanType T, ExplosionKind explosion,
                                    unsigned uncurryLevel,
                                    bool mangleAsFunction) {
  // FIXME: Prefix?
  llvm::SaveAndRestore<unsigned> oldArchetypesDepth(ArchetypesDepth);
  bindGenericParameters(genericParams, /*mangle*/ true);

  if (mangleAsFunction)
    mangleFunctionType(cast<AnyFunctionType>(T), explosion, uncurryLevel);
  else
    mangleType(T, explosion, uncurryLevel);
}
        
static OperatorFixity getDeclFixity(ValueDecl *decl) {
  if (!decl->getName().isOperator())
    return OperatorFixity::NotOperator;
  if (decl->getAttrs().isPostfix())
    return OperatorFixity::Postfix;
  if (decl->getAttrs().isPrefix())
    return OperatorFixity::Prefix;
  return OperatorFixity::Infix;
}

void Mangler::mangleDeclName(ValueDecl *decl, IncludeType includeType) {
  // decl ::= context identifier
  mangleContextOf(decl);
  mangleIdentifier(decl->getName(), getDeclFixity(decl));

  if (includeType == IncludeType::No) return;

  // We mangle the type with a canonical set of parameters because
  // objects nested within functions are shared across all expansions
  // of the function.
  mangleDeclType(decl, ExplosionKind::Minimal, /*uncurry*/ 0);
}

void Mangler::mangleDeclType(ValueDecl *decl, ExplosionKind explosion,
                             unsigned uncurryLevel) {
  // The return value here is a pair of (1) whether we need to mangle
  // the type and (2) whether we need to specifically bind parameters
  // from the context.
  typedef std::pair<bool, bool> result_t;
  struct ClassifyDecl : swift::DeclVisitor<ClassifyDecl, result_t> {
    /// TypeDecls don't need their types mangled in.
    result_t visitTypeDecl(TypeDecl *D) {
      return { false, false };
    }

    /// Function-like declarations do, but they should have
    /// polymorphic type and therefore don't need specific binding.
    result_t visitFuncDecl(FuncDecl *D) {
      return { true, false };
    }
    result_t visitConstructorDecl(ConstructorDecl *D) {
      return { true, false };
    }
    result_t visitDestructorDecl(DestructorDecl *D) {
      return { true, false };
    }
    result_t visitEnumElementDecl(EnumElementDecl *D) {
      return { true, false };
    }

    /// All other values need to have contextual archetypes bound.
    result_t visitVarDecl(VarDecl *D) {
      return { true, true };
    }
    result_t visitSubscriptDecl(SubscriptDecl *D) {
      return { true, true };
    }

    /// Make sure we have a case for every ValueDecl.
    result_t visitValueDecl(ValueDecl *D) = delete;

    /// Everything else should be unreachable here.
    result_t visitDecl(Decl *D) {
      llvm_unreachable("not a ValueDecl");
    }
  };

  auto result = ClassifyDecl().visit(decl);
  assert(result.first || !result.second);

  DeclCtx = decl->getDeclContext();

  // Bind the contextual archetypes if requested.
  llvm::SaveAndRestore<unsigned> oldArchetypesDepth(ArchetypesDepth);
  if (result.second) {
    auto genericParams = DeclCtx->getGenericParamsOfContext();
    if (genericParams) {
      bindGenericParameters(genericParams);

      // Let DeclCtx point to the parent innermost generic context.
      while (!DeclCtx->isInnermostContextGeneric())
        DeclCtx = DeclCtx->getParent();
    }
  }

  // Mangle the type if requested.
  if (result.first) {
    mangleType(decl->getType()->getCanonicalType(), explosion, uncurryLevel);
  }
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
/// <type> ::= Bv <natural> <type>   # Builtin.Vector
/// <type> ::= C <decl>              # class (substitutable)
/// <type> ::= ERR                   # Error type
/// <type> ::= F <type> <type>       # function type
/// <type> ::= f <type> <type>       # uncurried function type
/// <type> ::= G <type> <type>+ _    # bound generic type
/// <type> ::= O <decl>              # enum (substitutable)
/// <type> ::= P <protocol-list> _   # protocol composition
/// <type> ::= Q <index>             # archetype with depth=0, index=N
/// <type> ::= Qd <index> <index>    # archetype with depth=M+1, index=N
/// <type> ::= 'Qq' index context    # archetype+context (DWARF only)
///
/// <type> ::= R <type>              # lvalue
/// <type> ::= T <tuple-element>* _  # tuple
/// <type> ::= U <generic-parameter>+ _ <type>
/// <type> ::= V <decl>              # struct (substitutable)
/// <type> ::= Xo <type>             # unowned reference type
/// <type> ::= Xw <type>             # weak reference type
///
/// <index> ::= _                    # 0
/// <index> ::= <natural> _          # N+1
///
/// <tuple-element> ::= <identifier>? <type>
void Mangler::mangleType(CanType type, ExplosionKind explosion,
                         unsigned uncurryLevel) {
  switch (type->getKind()) {
  case TypeKind::TypeVariable:
    llvm_unreachable("mangling type variable");

  case TypeKind::Module:
    llvm_unreachable("Cannot mangle module type yet");

  case TypeKind::SILFunction:
    llvm_unreachable("SILFunctionType in mangler?");

  case TypeKind::Error:
    Buffer << "ERR";
    return;
      
  // We don't care about these types being a bit verbose because we
  // don't expect them to come up that often in API names.
  case TypeKind::BuiltinFloat:
    switch (cast<BuiltinFloatType>(type)->getFPKind()) {
    case BuiltinFloatType::IEEE16: Buffer << "Bf16_"; return;
    case BuiltinFloatType::IEEE32: Buffer << "Bf32_"; return;
    case BuiltinFloatType::IEEE64: Buffer << "Bf64_"; return;
    case BuiltinFloatType::IEEE80: Buffer << "Bf80_"; return;
    case BuiltinFloatType::IEEE128: Buffer << "Bf128_"; return;
    case BuiltinFloatType::PPC128: llvm_unreachable("ppc128 not supported");
    }
    llvm_unreachable("bad floating-point kind");
  case TypeKind::BuiltinInteger: {
    auto width = cast<BuiltinIntegerType>(type)->getWidth();
    if (width.isFixedWidth())
      Buffer << "Bi" << width.getFixedWidth() << '_';
    else if (width.isPointerWidth())
      Buffer << "Bw";
    else
      llvm_unreachable("impossible width value");
    return;
  }
  case TypeKind::BuiltinRawPointer:
    Buffer << "Bp";
    return;
  case TypeKind::BuiltinObjectPointer:
    Buffer << "Bo";
    return;
  case TypeKind::BuiltinObjCPointer:
    Buffer << "BO";
    return;
  case TypeKind::BuiltinVector:
    Buffer << "Bv" << cast<BuiltinVectorType>(type)->getNumElements();
    mangleType(cast<BuiltinVectorType>(type).getElementType(), explosion,
               uncurryLevel);
    return;

#define SUGARED_TYPE(id, parent) \
  case TypeKind::id: \
    llvm_unreachable("expect canonical type");
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"

  case TypeKind::MetaType:
    Buffer << 'M';
    return mangleType(cast<MetaTypeType>(type).getInstanceType(),
                      ExplosionKind::Minimal, 0);

  case TypeKind::LValue:
    Buffer << 'R';
    return mangleType(cast<LValueType>(type).getObjectType(),
                      ExplosionKind::Minimal, 0);

  case TypeKind::UnownedStorage:
    Buffer << "Xo";
    return mangleType(cast<UnownedStorageType>(type).getReferentType(),
                      ExplosionKind::Minimal, 0);

  case TypeKind::WeakStorage:
    Buffer << "Xw";
    return mangleType(cast<WeakStorageType>(type).getReferentType(),
                      ExplosionKind::Minimal, 0);

  case TypeKind::Tuple: {
    auto tuple = cast<TupleType>(type);
    // type ::= 'T' tuple-field+ '_'  // tuple
    // type ::= 't' tuple-field+ '_'  // variadic tuple
    // tuple-field ::= identifier? type
    if (tuple->getFields().size() > 0
        && tuple->getFields().back().isVararg())
      Buffer << 't';
    else
      Buffer << 'T';
    for (auto &field : tuple->getFields()) {
      if (field.hasName())
        mangleIdentifier(field.getName());
      mangleType(CanType(field.getType()), explosion, 0);
    }
    Buffer << '_';
    return;
  }

  case TypeKind::Enum:
    return mangleNominalType(cast<EnumType>(type)->getDecl(), explosion);

  case TypeKind::Protocol:
    // Protocol type manglings have a variable number of protocol names
    // follow the 'P' sigil, so a trailing underscore is needed after the
    // type name, unlike protocols as contexts.
    Buffer << 'P';
    mangleProtocolList(type);
    Buffer << '_';
    return;

  case TypeKind::Struct:
    return mangleNominalType(cast<StructType>(type)->getDecl(), explosion);

  case TypeKind::Class:
    return mangleNominalType(cast<ClassType>(type)->getDecl(), explosion);

  case TypeKind::UnboundGeneric:
    // We normally reject unbound types in IR-generation, but there
    // are several occasions in which we'd like to mangle them in the
    // abstract.
    mangleNominalType(cast<UnboundGenericType>(type)->getDecl(), explosion);
    return;

  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct: {
    // type ::= 'G' <type> <type>+ '_'
    auto boundType = cast<BoundGenericType>(type);
    Buffer << 'G';
    mangleNominalType(boundType->getDecl(), explosion);
    for (auto arg : boundType.getGenericArgs()) {
      mangleType(arg, ExplosionKind::Minimal, /*uncurry*/ 0);
    }
    Buffer << '_';
    return;
  }

  case TypeKind::PolymorphicFunction: {
    // <type> ::= U <generic-parameter>+ _ <type>
    // 'U' is for "universal qualification".
    // The nested type is always a function type.
    auto fn = cast<PolymorphicFunctionType>(type);
    Buffer << 'U';
    manglePolymorphicType(&fn->getGenericParams(), fn, explosion, uncurryLevel,
                          /*mangleAsFunction=*/true);
    return;
  }

  // type ::= archetype
  case TypeKind::Archetype: {
    auto archetype = cast<ArchetypeType>(type);
    
    // archetype ::= associated-type
    
    // associated-type ::= substitution
    if (tryMangleSubstitution(archetype.getPointer()))
      return;

    Buffer << 'Q';
    
    // associated-type ::= 'Q' archetype identifier
    // Mangle the associated type of a parent archetype.
    if (auto parent = archetype->getParent()) {
      assert(archetype->getAssocType()
             && "child archetype has no associated type?!");

      mangleType(CanType(parent), explosion, 0);
      mangleIdentifier(archetype->getName());
      addSubstitution(archetype.getPointer());
      return;
    }
    
    // associated-type ::= 'Q' protocol-context
    // Mangle the Self archetype of a protocol.
    if (archetype->getSelfProtocol()) {
      Buffer << 'P';
      mangleProtocolName(archetype->getAssocType()->getProtocol());
      addSubstitution(archetype.getPointer());
      return;
    }
    
    // archetype ::= 'Q' <index>             # archetype with depth=0, index=N
    // archetype ::= 'Qd' <index> <index>    # archetype with depth=M+1, index=N
    // Mangle generic parameter archetypes.

    // Find the archetype information.
    auto it = Archetypes.find(archetype);
    while (it == Archetypes.end()) {
      // This should be treated like an error, but we don't want
      // clients like lldb to crash because of corrupted input.
      assert(DeclCtx && "empty decl context");
      if (!DeclCtx) return;

      // This Archetype comes from an enclosing context -- proceed to
      // bind the generic params form all parent contexts.
      GenericParamList *GenericParams = nullptr;
      do { // Skip over empty parent contexts.
        DeclCtx = DeclCtx->getParent();
        assert(DeclCtx && "no decl context for archetype found");
        if (!DeclCtx) return;
        GenericParams = DeclCtx->getGenericParamsOfContext();
      } while (!GenericParams);

      bindGenericParameters(GenericParams);
      it = Archetypes.find(archetype);
    }
    auto &info = it->second;
    assert(ArchetypesDepth >= info.Depth);

    if (MangleArchetypesWithContext) {
      // The DWARF output created by swift is intentionally flat,
      // therefore archetypes are emitted with their DeclContext.
      Buffer << 'q' << Index(info.Index);
      mangleDeclContext(DeclCtx);
    } else {
      unsigned relativeDepth = ArchetypesDepth - info.Depth;
      if (relativeDepth != 0) {
        Buffer << 'd' << Index(relativeDepth - 1);
      }
      Buffer << Index(info.Index);
    }
    return;
  }

  case TypeKind::GenericFunction: {
    llvm_unreachable("cannot mangle generic function types yet");
  }

  case TypeKind::GenericTypeParam: {
    llvm_unreachable("cannot mangle generic type parameters yet");
  }

  case TypeKind::DependentMember: {
    llvm_unreachable("cannot mangle dependent member types yet");
  }

  case TypeKind::Function:
    mangleFunctionType(cast<FunctionType>(type), explosion, uncurryLevel);
    return;

  case TypeKind::Array: {
    // type ::= 'A' integer type
    auto array = cast<ArrayType>(type);
    Buffer << 'A';
    Buffer << array->getSize();
    mangleType(array.getBaseType(), ExplosionKind::Minimal, 0);
    return;
  };

  case TypeKind::ProtocolComposition: {
    // We mangle ProtocolType and ProtocolCompositionType using the
    // same production:
    //   <type> ::= P <protocol-list> _

    auto protocols = cast<ProtocolCompositionType>(type)->getProtocols();
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
  case DeclKind::Enum: return 'O';
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
  DeclContext *dc = decl->getDeclContext();
  if (!dc->isModuleScopeContext() || !dc->getParentModule()->isStdlibModule())
    return false;

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
  } else if (name == "Float64") {
    Buffer << "Sd";
    return true;
  } else if (name == "Float32") {
    Buffer << "Sf";
    return true;
  } else if (name == "Optional") {
    Buffer << "Sq";
    return true;
  } else if (name == "Slice") {
    Buffer << "Sa";
    return true;
  } else if (name == "String") {
    Buffer << "SS";
    return true;
  } else {
    return false;
  }
}

void Mangler::mangleFunctionType(CanAnyFunctionType fn,
                                 ExplosionKind explosion,
                                 unsigned uncurryLevel) {
  // type ::= 'F' type type (curried)
  // type ::= 'f' type type (uncurried)
  // type ::= 'b' type type (objc block)
  if (fn->isBlock())
    Buffer << 'b';
  else
    Buffer << (uncurryLevel > 0 ? 'f' : 'F');
  mangleType(fn.getInput(), explosion, 0);
  mangleType(fn.getResult(), explosion,
             (uncurryLevel > 0 ? uncurryLevel - 1 : 0));
}

void Mangler::mangleEntity(ValueDecl *decl, ExplosionKind explosion,
                           unsigned uncurryLevel) {
  mangleDeclName(decl, IncludeType::No);

  // Mangle in a type as well.  Note that we have to mangle the type
  // on all kinds of declarations, even variables, because at the
  // moment they can *all* be overloaded.
  mangleDeclType(decl, explosion, uncurryLevel);
}

void Mangler::mangleDirectness(bool isIndirect) {
  Buffer << (isIndirect ? 'i': 'd');
}

void Mangler::mangleProtocolConformance(ProtocolConformance *conformance) {
  // protocol-conformance ::= type protocol module
  // FIXME: explosion level?
  mangleType(conformance->getType()->getCanonicalType(),
             ExplosionKind::Minimal, 0);
  mangleProtocolName(conformance->getProtocol());
  mangleDeclContext(conformance->getContainingModule());
}
