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
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/Punycode.h"
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
void Mangler::mangleContextOf(ValueDecl *decl, BindGenerics shouldBind) {
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
  // Do a dance to avoid a layering dependency.
  if (auto file = dyn_cast<FileUnit>(decl->getDeclContext())) {
    if (file->getKind() == FileUnitKind::ClangModule) {
      Buffer << "SC";
      return;
    }
  }

  // Just mangle the decl's DC.
  mangleContext(decl->getDeclContext(), shouldBind);
}

namespace {
  class FindFirstVariable :
    public PatternVisitor<FindFirstVariable, VarDecl *> {
  public:
    VarDecl *visitNamedPattern(NamedPattern *P) {
      return P->getDecl();
    }

    VarDecl *visitTuplePattern(TuplePattern *P) {
      for (auto &elt : P->getFields()) {
        VarDecl *var = visit(elt.getPattern());
        if (var) return var;
      }
      return nullptr;
    }

    VarDecl *visitParenPattern(ParenPattern *P) {
      return visit(P->getSubPattern());
    }
    VarDecl *visitVarPattern(VarPattern *P) {
      return visit(P->getSubPattern());
    }
    VarDecl *visitTypedPattern(TypedPattern *P) {
      return visit(P->getSubPattern());
    }
    VarDecl *visitAnyPattern(AnyPattern *P) {
      return nullptr;
    }

    // Refutable patterns shouldn't ever come up.
#define REFUTABLE_PATTERN(ID, BASE)                                        \
    VarDecl *visit##ID##Pattern(ID##Pattern *P) {                          \
      llvm_unreachable("shouldn't be visiting a refutable pattern here!"); \
    }
#define PATTERN(ID, BASE)
#include "swift/AST/PatternNodes.def"
  };
}

/// Find the first identifier bound by the given binding.  This
/// assumes that field and global-variable bindings always bind at
/// least one name, which is probably a reasonable assumption but may
/// not be adequately enforced.
static VarDecl *findFirstVariable(PatternBindingDecl *binding) {
  auto var = FindFirstVariable().visit(binding->getPattern());
  assert(var && "pattern-binding bound no variables?");
  return var;
}

void Mangler::mangleContext(DeclContext *ctx, BindGenerics shouldBind) {
  switch (ctx->getContextKind()) {
  case DeclContextKind::Module:
    return mangleModule(cast<Module>(ctx));

  case DeclContextKind::FileUnit:
    assert(!isa<BuiltinUnit>(ctx) && "mangling member of builtin module!");
    mangleContext(ctx->getParent(), shouldBind);
    return;

  case DeclContextKind::NominalTypeDecl:
    mangleNominalType(cast<NominalTypeDecl>(ctx), ExplosionKind::Minimal,
                      shouldBind);
    return;

  case DeclContextKind::ExtensionDecl: {
    auto ExtD = cast<ExtensionDecl>(ctx);
    auto ExtTy = ExtD->getExtendedType();
    // Recover from erroneous extension.
    if (ExtTy->is<ErrorType>())
      return mangleContext(ExtD->getDeclContext(), shouldBind);

    auto decl = ExtTy->getAnyNominal();
    assert(decl && "extension of non-nominal type?");
    mangleNominalType(decl, ExplosionKind::Minimal, shouldBind);
    return;
  }

  case DeclContextKind::AbstractClosureExpr:
    return mangleClosureEntity(cast<AbstractClosureExpr>(ctx),
                               ExplosionKind::Minimal, /*uncurry*/ 0);

  case DeclContextKind::AbstractFunctionDecl: {
    auto fn = cast<AbstractFunctionDecl>(ctx);

    // Constructors and destructors as contexts are always mangled
    // using the non-(de)allocating variants.
    if (auto ctor = dyn_cast<ConstructorDecl>(fn)) {
      return mangleConstructorEntity(ctor, /*allocating*/ false,
                                     ExplosionKind::Minimal,
                                     /*uncurry*/ 0);
    } else if (auto dtor = dyn_cast<DestructorDecl>(fn)) {
      return mangleDestructorEntity(dtor, /*deallocating*/ false);
    } else if (auto func = dyn_cast<FuncDecl>(fn)) {
      if (auto value = func->getGetterDecl()) {
        return mangleGetterEntity(value, ExplosionKind::Minimal);
      } else if (auto value = func->getSetterDecl()) {
        return mangleSetterEntity(value, ExplosionKind::Minimal);
      }
    }
    return mangleEntity(fn, ExplosionKind::Minimal, /*uncurry*/ 0);
  }

  case DeclContextKind::Initializer:
    switch (cast<Initializer>(ctx)->getInitializerKind()) {
    case InitializerKind::DefaultArgument: {
      auto argInit = cast<DefaultArgumentInitializer>(ctx);
      mangleDefaultArgumentEntity(ctx->getParent(),
                                  argInit->getIndex());
      return;
    }

    case InitializerKind::PatternBinding: {
      auto patternInit = cast<PatternBindingInitializer>(ctx);
      auto var = findFirstVariable(patternInit->getBinding());
      mangleInitializerEntity(var);
      return;
    }
    }
    llvm_unreachable("bad initializer kind");

  case DeclContextKind::TopLevelCodeDecl:
    // Mangle the containing module context.
    return mangleContext(ctx->getParent(), shouldBind);
  }

  llvm_unreachable("bad decl context");
}

void Mangler::mangleModule(Module *module) {
  assert(!module->getParent() && "cannot mangle nested modules!");

  // Try the special 'swift' substitution.
  // context ::= known-module
  // known-module ::= 'Ss'
  if (module->isStdlibModule()) {
    Buffer << "Ss";
    return;
  }

  // context ::= substitution
  if (tryMangleSubstitution(module)) return;

  // context ::= identifier
  mangleIdentifier(module->Name);

  addSubstitution(module);
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
  assert(ArchetypesDepth == genericParams->getDepth());
  ArchetypesDepth++;
  unsigned index = 0;
  for (auto archetype : genericParams->getPrimaryArchetypes()) {
    // Remember the current depth and level.
    ArchetypeInfo info;
    info.Depth = ArchetypesDepth;
    info.Index = index++;
    assert(!Archetypes.count(archetype) ||
           (Archetypes[archetype].Depth == info.Depth &&
            Archetypes[archetype].Index == info.Index));
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

void Mangler::mangleDeclName(ValueDecl *decl) {
  // decl-name ::= 'L' index identifier
  if (decl->getDeclContext()->isLocalContext()) {
    Buffer << 'L' << Index(decl->getLocalDiscriminator());
    // Fall through to mangle the <identiifier>.
  }

  // decl-name ::= identifier
  mangleIdentifier(decl->getName(), getDeclFixity(decl));
}

static void bindAllGenericParameters(Mangler &mangler,
                                     GenericParamList *generics) {
  if (!generics) return;
  bindAllGenericParameters(mangler, generics->getOuterParameters());
  mangler.bindGenericParameters(generics, /*mangle*/ false);
}

void Mangler::mangleTypeForDebugger(Type Ty, DeclContext *DC) {
  assert(DWARFMangling);

  if (auto NameAliasTy = dyn_cast<NameAliasType>(Ty.getPointer())) {
    TypeAliasDecl *decl = NameAliasTy->getDecl();
    assert(decl);

    if (decl->getModuleContext() == decl->getASTContext().TheBuiltinModule) {
      // It's not possible to mangle the context of the builtin module.
      Buffer << decl->getName().str();
    } else {
      // For the DWARF output we want to mangle the type alias + context,
      // unless the type alias references a builtin type.
      assert(decl->getModuleContext() !=
             decl->getASTContext().TheBuiltinModule);
      ContextStack context(*this);
      Buffer << "_Tta";
      mangleContextOf(decl, BindGenerics::None);
      mangleIdentifier(decl->getName());
    }
    return;
  }

  Buffer << "_Tt";
  // Move up to the innermost generic context.
  while (DC && !DC->isInnermostContextGeneric()) DC = DC->getParent();
  if (DC) bindAllGenericParameters(*this, DC->getGenericParamsOfContext());
  DeclCtx = DC;

  mangleType(Ty->getCanonicalType(), ExplosionKind::Minimal, /*uncurry*/ 0);
}

void Mangler::mangleDeclTypeForDebugger(ValueDecl *decl) {
  assert(DWARFMangling);
  Buffer << "_Tt";

  typedef std::pair<bool, BindGenerics> result_t;
  struct ClassifyDecl : swift::DeclVisitor<ClassifyDecl, result_t> {

    /// TypeAliasDecls need to be mangled.
    result_t visitTypeAliasDecl(TypeDecl *D) {
      llvm_unreachable("filtered out above");
    }
    /// Other TypeDecls don't need their types mangled in.
    result_t visitTypeDecl(TypeDecl *D) {
      return { false, BindGenerics::None };
    }

    /// Function-like declarations do, but they should have
    /// polymorphic type and therefore don't need specific binding.
    result_t visitFuncDecl(FuncDecl *D) {
      if (D->getDeclContext()->isTypeContext())
        return { true, BindGenerics::Enclosing };
      else
        return { true, BindGenerics::All };
    }
    result_t visitConstructorDecl(ConstructorDecl *D) {
      return { true, BindGenerics::Enclosing };
    }
    result_t visitDestructorDecl(DestructorDecl *D) {
      return { true, BindGenerics::Enclosing };
    }
    result_t visitEnumElementDecl(EnumElementDecl *D) {
      return { true, BindGenerics::Enclosing };
    }

    /// All other values need to have contextual archetypes bound.
    result_t visitVarDecl(VarDecl *D) {
      return { true, BindGenerics::All };
    }
    result_t visitSubscriptDecl(SubscriptDecl *D) {
      return { true, BindGenerics::All };
    }

    /// Make sure we have a case for every ValueDecl.
    result_t visitValueDecl(ValueDecl *D) = delete;

    /// Everything else should be unreachable here.
    result_t visitDecl(Decl *D) {
      llvm_unreachable("not a ValueDecl");
    }
  };

  auto result = ClassifyDecl().visit(decl);
  if (!result.first) return;

  // Find the innermost generic context and stash it in DeclCtx.
  // Also track whether DC is just a type ancestor of the decl.
  DeclContext *DC = decl->getDeclContext();
  bool isNonTypeDC = false;
  while (DC && !DC->isInnermostContextGeneric()) {
    if (!isNonTypeDC) isNonTypeDC = !DC->isTypeContext();
    DC = DC->getParent();
  }
  DeclCtx = DC;

  // Bind the generic parameters from that.
  if (DC) {
    // But if the generics come from a type container, they may be
    // accounted for in the decl's type; skip them if so.
    if (result.second == BindGenerics::Enclosing && !isNonTypeDC) {
      while (DC->isTypeContext())
        DC = DC->getParent();
    }
    bindAllGenericParameters(*this, DC->getGenericParamsOfContext());
  }

  mangleDeclType(decl, ExplosionKind::Minimal, /*uncurry*/ 0);
}

void Mangler::mangleDeclType(ValueDecl *decl, ExplosionKind explosion,
                             unsigned uncurryLevel) {
  auto type = decl->getType();
  mangleType(type->getCanonicalType(), explosion, uncurryLevel);
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
/// <type> ::= 'a' <context> <identifier> # Type alias (DWARF only)
/// <type> ::= F <type> <type>       # function type
/// <type> ::= f <type> <type>       # uncurried function type
/// <type> ::= G <type> <type>+ _    # bound generic type
/// <type> ::= O <decl>              # enum (substitutable)
/// <type> ::= P <protocol-list> _   # protocol composition
/// <type> ::= Q <index>             # archetype with depth=0, index=N
/// <type> ::= Qd <index> <index>    # archetype with depth=M+1, index=N
/// <type> ::= 'Qq' index context    # archetype+context (DWARF only)
///
/// <type> ::= R <type>              # inout
/// <type> ::= T <tuple-element>* _  # tuple
/// <type> ::= U <generic-parameter>+ _ <type>
/// <type> ::= V <decl>              # struct (substitutable)
/// <type> ::= Xo <type>             # unowned reference type
/// <type> ::= Xw <type>             # weak reference type
/// <type> ::= XF <impl-function-type> # SIL function type
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

  case TypeKind::Metatype:
    Buffer << 'M';
    return mangleType(cast<MetatypeType>(type).getInstanceType(),
                      ExplosionKind::Minimal, 0);
  case TypeKind::LValue:
    assert(0 && "@lvalue types should not occur in function interfaces");
  case TypeKind::InOut:
    Buffer << 'R';
    return mangleType(cast<InOutType>(type).getObjectType(),
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

  case TypeKind::Protocol:
    // Protocol type manglings have a variable number of protocol names
    // follow the 'P' sigil, so a trailing underscore is needed after the
    // type name, unlike protocols as contexts.
    Buffer << 'P';
    mangleProtocolList(type);
    Buffer << '_';
    return;

  case TypeKind::UnboundGeneric: {
    // We normally reject unbound types in IR-generation, but there
    // are several occasions in which we'd like to mangle them in the
    // abstract.
    ContextStack context(*this);
    mangleNominalType(cast<UnboundGenericType>(type)->getDecl(), explosion,
                      BindGenerics::None);
    return;
  }

  case TypeKind::Class:
  case TypeKind::Enum:
  case TypeKind::Struct: {
    ContextStack context(*this);
    return mangleNominalType(cast<NominalType>(type)->getDecl(), explosion,
                             BindGenerics::None);
  }

  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct: {
    // type ::= 'G' <type> <type>+ '_'
    auto boundType = cast<BoundGenericType>(type);
    Buffer << 'G';
    {
      ContextStack context(*this);
      mangleNominalType(boundType->getDecl(), explosion, BindGenerics::None);
    }
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

  case TypeKind::SILFunction: {
    // <type> ::= 'XF' <impl-function-type>
    // <impl-function-type> ::= <impl-callee-convention>
    //                          <impl-function-attribute>* <generics>? '_'
    //                          <impl-parameter>* '_' <impl-result>* '_'
    // <impl-callee-convention> ::= 't'               // thin
    // <impl-callee-convention> ::= <impl-convention> // thick
    // <impl-convention> ::= 'a'                      // direct, autoreleased
    // <impl-convention> ::= 'd'                      // direct, no ownership transfer
    // <impl-convention> ::= 'g'                      // direct, guaranteed
    // <impl-convention> ::= 'i'                      // indirect, ownership transfer
    // <impl-convention> ::= 'l'                      // indirect, inout
    // <impl-convention> ::= 'o'                      // direct, ownership transfer
    // <impl-function-attribute> ::= 'Cb'             // block invocation function
    // <impl-function-attribute> ::= 'Cc'             // C global function
    // <impl-function-attribute> ::= 'Cm'             // Swift method
    // <impl-function-attribute> ::= 'CO'             // ObjC method
    // <impl-function-attribute> ::= 'N'              // noreturn
    // <impl-function-attribute> ::= 'G'              // generic
    // <impl-parameter> ::= <impl-convention> <type>
    // <impl-result> ::= <impl-convention> <type>
    auto fn = cast<SILFunctionType>(type);
    Buffer << "XF";

    auto mangleParameterConvention = [](ParameterConvention conv) {
      // @in and @out are mangled the same because they're put in
      // different places.
      switch (conv) {
      case ParameterConvention::Indirect_In: return 'i';
      case ParameterConvention::Indirect_Out: return 'i';
      case ParameterConvention::Indirect_Inout: return 'l';
      case ParameterConvention::Direct_Owned: return 'o';
      case ParameterConvention::Direct_Unowned: return 'd';
      case ParameterConvention::Direct_Guaranteed: return 'g';
      }
      llvm_unreachable("bad parameter convention");
    };
    auto mangleResultConvention = [](ResultConvention conv) {
      switch (conv) {
      case ResultConvention::Owned: return 'o';
      case ResultConvention::Unowned: return 'd';
      case ResultConvention::Autoreleased: return 'a';
      }
      llvm_unreachable("bad result convention");
    };

    // <impl-callee-convention>
    if (fn->isThin()) {
      Buffer << 't';
    } else {
      Buffer << mangleParameterConvention(fn->getCalleeConvention());
    }

    // <impl-function-attribute>*
    if (fn->isBlock()) {
      Buffer << "Cb";
    } else {
      switch (fn->getAbstractCC()) {
      case AbstractCC::Freestanding: break;
      case AbstractCC::C: Buffer << "Cc"; break;
      case AbstractCC::ObjCMethod: Buffer << "CO"; break;
      case AbstractCC::Method: Buffer << "Cm"; break;
      case AbstractCC::WitnessMethod: Buffer << "Cw"; break;
      }
    }
    if (fn->isNoReturn()) Buffer << 'N';
    if (fn->isPolymorphic()) {
      Buffer << 'G';
      bindGenericParameters(fn->getGenericParams(), /*mangle*/ true);
    }
    Buffer << '_';

    auto mangleParameter = [&](SILParameterInfo param) {
      Buffer << mangleParameterConvention(param.getConvention());
      mangleType(param.getType(), ExplosionKind::Minimal, 0);
    };

    for (auto param : fn->getParametersWithoutIndirectResult()) {
      mangleParameter(param);
    }
    Buffer << '_';

    if (fn->hasIndirectResult()) {
      mangleParameter(fn->getIndirectResult());
    } else {
      auto result = fn->getResult();
      Buffer << mangleResultConvention(result.getConvention());
      mangleType(result.getType(), ExplosionKind::Minimal, 0);
    }
    Buffer << '_';
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
    if (auto proto = archetype->getSelfProtocol()) {
      Buffer << 'P';
      mangleProtocolName(proto);
      addSubstitution(archetype.getPointer());
      return;
    }
    
    // archetype ::= 'Q' <index>             # archetype with depth=0, index=N
    // archetype ::= 'Qd' <index> <index>    # archetype with depth=M+1, index=N
    // Mangle generic parameter archetypes.

    // Find the archetype information.
    DeclContext *DC = DeclCtx;
    auto it = Archetypes.find(archetype);
    while (it == Archetypes.end()) {
      // This should be treated like an error, but we don't want
      // clients like lldb to crash because of corrupted input.
      assert(DC && "empty decl context");
      if (!DC) return;

      // This Archetype comes from an enclosing context -- proceed to
      // bind the generic params form all parent contexts.
      GenericParamList *GenericParams = nullptr;
      do { // Skip over empty parent contexts.
        DC = DC->getParent();
        assert(DC && "no decl context for archetype found");
        if (!DC) return;
        GenericParams = DC->getGenericParamsOfContext();
      } while (!GenericParams);

      bindGenericParameters(GenericParams);
      it = Archetypes.find(archetype);
    }
    auto &info = it->second;
    assert(ArchetypesDepth >= info.Depth);
    unsigned relativeDepth = ArchetypesDepth - info.Depth;

    if (DWARFMangling) {
      Buffer << 'q' << Index(info.Index);
      // The DWARF output created by Swift is intentionally flat,
      // therefore archetypes are emitted with their DeclContext if
      // they appear at the top level of a type (_Tt).
      // Clone a new, non-DWARF Mangler for the DeclContext.
      Mangler ContextMangler(Buffer, /*DWARFMangling=*/false);
      SmallVector<void *, 4> SortedSubsts(Substitutions.size());
      for (auto S : Substitutions) SortedSubsts[S.second] = S.first;
      for (auto S : SortedSubsts) ContextMangler.addSubstitution(S);
      for (; relativeDepth > 0; --relativeDepth)
        DC = DC->getParent();
      ContextMangler.mangleContext(DC, BindGenerics::None);
    } else {
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
  ContextStack context(*this);
  mangleContextOf(protocol, BindGenerics::None);
  mangleDeclName(protocol);
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
                                ExplosionKind explosion,
                                BindGenerics shouldBind) {
  auto bindGenericsIfDesired = [&] {
    if (shouldBind == BindGenerics::All)
      if (auto generics = decl->getGenericParams())
        bindGenericParameters(generics, /*mangle*/ false);
  };

  // Check for certain standard types.
  if (tryMangleStandardSubstitution(decl)) {
    bindGenericsIfDesired();
    return;
  }

  // For generic types, this uses the unbound type.
  TypeBase *key = decl->getDeclaredType().getPointer();

  // Try to mangle the entire name as a substitution.
  // type ::= substitution
  if (tryMangleSubstitution(key)) {
    bindGenericsIfDesired();
    return;
  }

  Buffer << getSpecifierForNominalType(decl);
  mangleContextOf(decl, shouldBind);
  bindGenericsIfDesired();
  mangleDeclName(decl);

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
  } else if (name == "UnicodeScalar") {
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
  } else if (name == "Array") {
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

void Mangler::mangleClosureEntity(AbstractClosureExpr *closure,
                                  ExplosionKind explosion,
                                  unsigned uncurryLevel) {
  // entity-name ::= 'U' index type         // explicit anonymous closure
  // entity-name ::= 'u' index type         // implicit anonymous closure

  auto discriminator = closure->getDiscriminator();
  assert(discriminator != AbstractClosureExpr::InvalidDiscriminator
         && "closure not marked correctly with discriminator?");

  Buffer << 'F';
  mangleContext(closure->getParent(), BindGenerics::All);
  Buffer << (isa<ClosureExpr>(closure) ? 'U' : 'u') << Index(discriminator);

  mangleType(closure->getType()->getCanonicalType(),
             ExplosionKind::Minimal, /*uncurry*/ 0);
}

void Mangler::mangleConstructorEntity(ConstructorDecl *ctor,
                                      bool isAllocating,
                                      ExplosionKind explosion,
                                      unsigned uncurryLevel) {
  Buffer << 'F';
  mangleContextOf(ctor, BindGenerics::Enclosing);
  Buffer << (isAllocating ? 'C' : 'c');
  mangleDeclType(ctor, explosion, uncurryLevel);
}

void Mangler::mangleDestructorEntity(DestructorDecl *dtor,
                                     bool isDeallocating) {
  Buffer << 'F';
  mangleContextOf(dtor, BindGenerics::Enclosing);
  Buffer << (isDeallocating ? 'D' : 'd');
}

void Mangler::mangleGetterEntity(ValueDecl *decl, ExplosionKind explosion) {
  Buffer << 'F';
  mangleContextOf(decl, BindGenerics::All);
  Buffer << 'g';
  mangleDeclName(decl);
  mangleDeclType(decl, explosion, 0);
}

void Mangler::mangleSetterEntity(ValueDecl *decl, ExplosionKind explosion) {
  Buffer << 'F';
  mangleContextOf(decl, BindGenerics::All);
  Buffer << 's';
  mangleDeclName(decl);
  mangleDeclType(decl, explosion, 0);
}

void Mangler::mangleAddressorEntity(ValueDecl *decl) {
  Buffer << 'F';
  mangleContextOf(decl, BindGenerics::All);
  Buffer << 'a';
  mangleDeclName(decl);
  mangleDeclType(decl, ExplosionKind::Minimal, 0);
}

void Mangler::mangleDefaultArgumentEntity(DeclContext *func, unsigned index) {
  Buffer << 'I';
  mangleContext(func, BindGenerics::All);
  Buffer << 'A' << Index(index);
}

void Mangler::mangleInitializerEntity(VarDecl *var) {
  // The initializer is its own entity whose context is the variable.
  Buffer << 'I';
  mangleEntity(var, ExplosionKind::Minimal, /*uncurry*/ 0);
  Buffer << 'i';
}

void Mangler::mangleEntity(ValueDecl *decl, ExplosionKind explosion,
                           unsigned uncurryLevel) {
  assert(!isa<ConstructorDecl>(decl));
  assert(!isa<DestructorDecl>(decl));
  assert(!isa<FuncDecl>(decl) || !cast<FuncDecl>(decl)->isGetterOrSetter());

  BindGenerics shouldBindParent = BindGenerics::All;

  // entity ::= entity-kind context entity-name
  if (isa<VarDecl>(decl)) {
    Buffer << 'v';
  } else {
    assert(isa<AbstractFunctionDecl>(decl) ||
           isa<EnumElementDecl>(decl));
    Buffer << 'F';

    // If this is a method, then its formal type includes the
    // archetypes of its parent.
    if (decl->getDeclContext()->isTypeContext())
      shouldBindParent = BindGenerics::Enclosing;
  }
  mangleContextOf(decl, shouldBindParent);
  mangleDeclName(decl);
  mangleDeclType(decl, explosion, uncurryLevel);
}

void Mangler::mangleDirectness(bool isIndirect) {
  Buffer << (isIndirect ? 'i': 'd');
}

void Mangler::mangleProtocolConformance(ProtocolConformance *conformance) {
  // protocol-conformance ::= ('U' generic-parameter+ '_')?
  //                          type protocol module
  // FIXME: explosion level?

  ContextStack context(*this);

  // If the conformance is generic, mangle its generic parameters.
  if (auto gp = conformance->getGenericParams()) {
    Buffer << 'U';
    bindGenericParameters(gp, /*mangle*/ true);
  }
  
  mangleType(conformance->getType()->getCanonicalType(),
             ExplosionKind::Minimal, 0);
  mangleProtocolName(conformance->getProtocol());
  mangleModule(conformance->getDeclContext()->getParentModule());
}
