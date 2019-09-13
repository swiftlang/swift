//===--- PrintAsObjC.cpp - Emit a header file for a Swift AST -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/PrintAsObjC/PrintAsObjC.h"

#include "DeclAndTypePrinter.h"

#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Module.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SwiftNameTranslation.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/Basic/Version.h"

#include "clang/AST/Decl.h"
#include "clang/Basic/Module.h"

#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace swift::objc_translation;

using DelayedMemberSet = DeclAndTypePrinter::DelayedMemberSet;

/// Returns true if \p decl represents an <os/object.h> type.
static bool isOSObjectType(const clang::Decl *decl) {
  auto *named = dyn_cast_or_null<clang::NamedDecl>(decl);
  if (!named)
    return false;
  return !DeclAndTypePrinter::maybeGetOSObjectBaseName(named).empty();
}

namespace {
class ReferencedTypeFinder : public TypeVisitor<ReferencedTypeFinder> {
  friend TypeVisitor;

  ModuleDecl &M;
  llvm::function_ref<void(ReferencedTypeFinder &, const TypeDecl *)> Callback;
  bool NeedsDefinition = false;

  ReferencedTypeFinder(ModuleDecl &mod, decltype(Callback) callback)
    : M(mod), Callback(callback) {}

  void visitType(TypeBase *base) {
    llvm_unreachable("unhandled type");
  }

  void visitTypeAliasType(TypeAliasType *aliasTy) {
    if (aliasTy->getDecl()->hasClangNode() &&
        !aliasTy->getDecl()->isCompatibilityAlias()) {
      Callback(*this, aliasTy->getDecl());
    } else {
      visit(aliasTy->getSinglyDesugaredType());
    }
  }

  void visitParenType(ParenType *parenTy) {
    visit(parenTy->getSinglyDesugaredType());
  }

  void visitTupleType(TupleType *tupleTy) {
    for (auto elemTy : tupleTy->getElementTypes())
      visit(elemTy);
  }

  void visitReferenceStorageType(ReferenceStorageType *ty) {
    visit(ty->getReferentType());
  }

  void visitNominalType(NominalType *nominal) {
    Callback(*this, nominal->getDecl());
  }

  void visitAnyMetatypeType(AnyMetatypeType *metatype) {
    visit(metatype->getInstanceType());
  }

  void visitDynamicSelfType(DynamicSelfType *module) {
    return;
  }

  void visitArchetypeType(ArchetypeType *archetype) {
    llvm_unreachable("Should not see archetypes in interface types");
  }

  void visitGenericTypeParamType(GenericTypeParamType *param) {
    // Appears in protocols and in generic ObjC classes.
    return;
  }

  void visitDependentMemberType(DependentMemberType *member) {
    // Appears in protocols and in generic ObjC classes.
    return;
  }

  void visitAnyFunctionType(AnyFunctionType *fnTy) {
    for (auto &param : fnTy->getParams())
      visit(param.getOldType());
    visit(fnTy->getResult());
  }

  void visitSyntaxSugarType(SyntaxSugarType *sugar) {
    visit(sugar->getSinglyDesugaredType());
  }

  void visitProtocolCompositionType(ProtocolCompositionType *composition) {
    auto layout = composition->getExistentialLayout();
    if (auto superclass = layout.explicitSuperclass)
      visit(superclass);
    for (auto proto : layout.getProtocols())
      visit(proto);
  }

  void visitLValueType(LValueType *lvalue) {
    llvm_unreachable("LValue types should not appear in interface types");
  }

  void visitInOutType(InOutType *inout) {
    visit(inout->getObjectType());
  }

  /// Returns true if \p paramTy has any constraints other than being
  /// class-bound ("conforms to" AnyObject).
  static bool isConstrained(GenericSignature *sig,
                            GenericTypeParamType *paramTy) {
    if (sig->getSuperclassBound(paramTy))
      return true;

    auto conformsTo = sig->getConformsTo(paramTy);
    return !conformsTo.empty();
  }

  void visitBoundGenericType(BoundGenericType *boundGeneric) {
    auto *decl = boundGeneric->getDecl();

    NeedsDefinition = true;
    Callback(*this, decl);
    NeedsDefinition = false;

    bool isObjCGeneric = decl->hasClangNode();
    auto *sig = decl->getGenericSignature();

    for_each(boundGeneric->getGenericArgs(),
             sig->getInnermostGenericParams(),
             [&](Type argTy, GenericTypeParamType *paramTy) {
      if (isObjCGeneric && isConstrained(sig, paramTy))
        NeedsDefinition = true;
      visit(argTy);
      NeedsDefinition = false;
    });
  }

public:
  using TypeVisitor::visit;

  bool needsDefinition() const {
    return NeedsDefinition;
  }

  static void walk(ModuleDecl &mod, Type ty, decltype(Callback) callback) {
    ReferencedTypeFinder(mod, callback).visit(ty);
  }
};

class ModuleWriter {
  enum class EmissionState {
    NotYetDefined = 0,
    DefinitionRequested,
    Defined
  };

  llvm::DenseMap<const TypeDecl *, std::pair<EmissionState, bool>> seenTypes;
  std::vector<const Decl *> declsToWrite;
  DelayedMemberSet delayedMembers;

  using ImportModuleTy = PointerUnion<ModuleDecl*, const clang::Module*>;
  SmallPtrSet<ImportModuleTy, 8> imports;

  std::string bodyBuffer;
  llvm::raw_string_ostream os{bodyBuffer};

  ModuleDecl &M;
  StringRef bridgingHeader;
  DeclAndTypePrinter printer;
public:
  ModuleWriter(ModuleDecl &mod, StringRef header, AccessLevel access)
    : M(mod), bridgingHeader(header), printer(M, os, delayedMembers, access) {}

  /// Returns true if we added the decl's module to the import set, false if
  /// the decl is a local decl.
  ///
  /// The standard library is special-cased: we assume that any types from it
  /// will be handled explicitly rather than needing an explicit @import.
  bool addImport(const Decl *D) {
    ModuleDecl *otherModule = D->getModuleContext();

    if (otherModule == &M)
      return false;
    if (otherModule->isStdlibModule() ||
        otherModule->isBuiltinModule())
      return true;
    // Don't need a module for SIMD types in C.
    if (otherModule->getName() == M.getASTContext().Id_simd)
      return true;

    // If there's a Clang node, see if it comes from an explicit submodule.
    // Import that instead, looking through any implicit submodules.
    if (auto clangNode = D->getClangNode()) {
      auto importer =
        static_cast<ClangImporter *>(M.getASTContext().getClangModuleLoader());
      if (const auto *clangModule = importer->getClangOwningModule(clangNode)) {
        while (clangModule && !clangModule->IsExplicit)
          clangModule = clangModule->Parent;
        if (clangModule) {
          imports.insert(clangModule);
          return true;
        }
      }
    }

    imports.insert(otherModule);
    return true;
  }

  bool hasBeenRequested(const TypeDecl *D) const {
    return seenTypes.lookup(D).first >= EmissionState::DefinitionRequested;
  }

  bool tryRequire(const TypeDecl *D) {
    if (addImport(D)) {
      seenTypes[D] = { EmissionState::Defined, true };
      return true;
    }
    auto &state = seenTypes[D];
    return state.first == EmissionState::Defined;
  }

  bool require(const TypeDecl *D) {
    if (addImport(D)) {
      seenTypes[D] = { EmissionState::Defined, true };
      return true;
    }

    auto &state = seenTypes[D];
    switch (state.first) {
    case EmissionState::NotYetDefined:
    case EmissionState::DefinitionRequested:
      state.first = EmissionState::DefinitionRequested;
      declsToWrite.push_back(D);
      return false;
    case EmissionState::Defined:
      return true;
    }

    llvm_unreachable("Unhandled EmissionState in switch.");
  }

  void forwardDeclare(const NominalTypeDecl *NTD,
                      llvm::function_ref<void(void)> Printer) {
    if (NTD->getModuleContext()->isStdlibModule())
      return;
    auto &state = seenTypes[NTD];
    if (state.second)
      return;
    Printer();
    state.second = true;
  }

  bool forwardDeclare(const ClassDecl *CD) {
    if (!CD->isObjC() ||
        CD->getForeignClassKind() == ClassDecl::ForeignKind::CFType ||
        isOSObjectType(CD->getClangDecl())) {
      return false;
    }
    forwardDeclare(CD, [&]{ os << "@class " << getNameForObjC(CD) << ";\n"; });
    return true;
  }

  void forwardDeclare(const ProtocolDecl *PD) {
    assert(PD->isObjC() ||
           *PD->getKnownProtocolKind() == KnownProtocolKind::Error);
    forwardDeclare(PD, [&]{
      os << "@protocol " << getNameForObjC(PD) << ";\n";
    });
  }
  
  void forwardDeclare(const EnumDecl *ED) {
    assert(ED->isObjC() || ED->hasClangNode());
    
    forwardDeclare(ED, [&]{
      os << "enum " << getNameForObjC(ED) << " : ";
      printer.print(ED->getRawType());
      os << ";\n";
    });
  }

  bool forwardDeclareMemberTypes(DeclRange members, const Decl *container) {
    switch (container->getKind()) {
    case DeclKind::Class:
    case DeclKind::Protocol:
    case DeclKind::Extension:
      break;
    default:
      llvm_unreachable("unexpected container kind");
    }

    bool hadAnyDelayedMembers = false;
    SmallVector<ValueDecl *, 4> nestedTypes;
    for (auto member : members) {
      auto VD = dyn_cast<ValueDecl>(member);
      if (!VD || !printer.shouldInclude(VD))
        continue;

      // Catch nested types and emit their definitions /after/ this class.
      if (isa<TypeDecl>(VD)) {
        // Don't emit nested types that are just implicitly @objc.
        // You should have to opt into this, since they are even less
        // namespaced than usual.
        if (std::any_of(VD->getAttrs().begin(), VD->getAttrs().end(),
                        [](const DeclAttribute *attr) {
                          return isa<ObjCAttr>(attr) && !attr->isImplicit();
                        })) {
          nestedTypes.push_back(VD);
        }
        continue;
      }

      bool needsToBeIndividuallyDelayed = false;
      ReferencedTypeFinder::walk(M, VD->getInterfaceType(),
                                 [&](ReferencedTypeFinder &finder,
                                     const TypeDecl *TD) {
        if (TD == container)
          return;

        if (finder.needsDefinition() && isa<NominalTypeDecl>(TD)) {
          // We can delay individual members of classes; do so if necessary.
          if (isa<ClassDecl>(container)) {
            if (!tryRequire(TD)) {
              needsToBeIndividuallyDelayed = true;
              hadAnyDelayedMembers = true;
            }
            return;
          }

          // Extensions can always be delayed wholesale.
          if (isa<ExtensionDecl>(container)) {
            if (!require(TD))
              hadAnyDelayedMembers = true;
            return;
          }

          // Protocols should be delayed wholesale unless we might have a cycle.
          auto *proto = cast<ProtocolDecl>(container);
          if (!hasBeenRequested(proto) || !hasBeenRequested(TD)) {
            if (!require(TD))
              hadAnyDelayedMembers = true;
            return;
          }

          // Otherwise, we have a cyclic dependency. Give up and continue with
          // regular forward-declarations even though this will lead to an
          // error; there's nothing we can do here.
          // FIXME: It would be nice to diagnose this.
        }

        if (auto CD = dyn_cast<ClassDecl>(TD)) {
          if (!forwardDeclare(CD)) {
            (void)addImport(CD);
          }
        } else if (auto PD = dyn_cast<ProtocolDecl>(TD)) {
          forwardDeclare(PD);
        } else if (auto TAD = dyn_cast<TypeAliasDecl>(TD)) {
          if (TAD->hasClangNode())
            (void)addImport(TD);
        } else if (addImport(TD)) {
          return;
        } else if (auto ED = dyn_cast<EnumDecl>(TD)) {
          forwardDeclare(ED);
        } else if (isa<AbstractTypeParamDecl>(TD)) {
          llvm_unreachable("should not see type params here");
        } else {
          assert(false && "unknown local type decl");
        }
      });

      if (needsToBeIndividuallyDelayed) {
        assert(isa<ClassDecl>(container));
        delayedMembers.insert(VD);
      }
    }

    declsToWrite.insert(declsToWrite.end()-1, nestedTypes.rbegin(),
                        nestedTypes.rend());

    // Separate forward declarations from the class itself.
    return !hadAnyDelayedMembers;
  }

  bool writeClass(const ClassDecl *CD) {
    if (addImport(CD))
      return true;

    if (seenTypes[CD].first == EmissionState::Defined)
      return true;

    bool allRequirementsSatisfied = true;

    const ClassDecl *superclass = nullptr;
    if ((superclass = CD->getSuperclassDecl())) {
      allRequirementsSatisfied &= require(superclass);
    }
    for (auto proto : CD->getLocalProtocols(
                        ConformanceLookupKind::OnlyExplicit))
      if (printer.shouldInclude(proto))
        allRequirementsSatisfied &= require(proto);

    if (!allRequirementsSatisfied)
      return false;

    (void)forwardDeclareMemberTypes(CD->getMembers(), CD);
    seenTypes[CD] = { EmissionState::Defined, true };
    os << '\n';
    printer.print(CD);
    return true;
  }
  
  bool writeFunc(const FuncDecl *FD) {
    if (addImport(FD))
      return true;

    printer.print(FD);
    return true;
  }

  bool writeProtocol(const ProtocolDecl *PD) {
    if (addImport(PD))
      return true;

    if (seenTypes[PD].first == EmissionState::Defined)
      return true;

    bool allRequirementsSatisfied = true;

    for (auto proto : PD->getInheritedProtocols()) {
      assert(proto->isObjC());
      allRequirementsSatisfied &= require(proto);
    }

    if (!allRequirementsSatisfied)
      return false;

    if (!forwardDeclareMemberTypes(PD->getMembers(), PD))
      return false;

    seenTypes[PD] = { EmissionState::Defined, true };
    os << '\n';
    printer.print(PD);
    return true;
  }

  bool writeExtension(const ExtensionDecl *ED) {
    bool allRequirementsSatisfied = true;

    const ClassDecl *CD = ED->getSelfClassDecl();
    allRequirementsSatisfied &= require(CD);
    for (auto proto : ED->getLocalProtocols())
      if (printer.shouldInclude(proto))
        allRequirementsSatisfied &= require(proto);

    if (!allRequirementsSatisfied)
      return false;

    // This isn't rolled up into the previous set of requirements because
    // it /also/ prints forward declarations, and the header is a little
    // prettier if those are as close as possible to the necessary extension.
    if (!forwardDeclareMemberTypes(ED->getMembers(), ED))
      return false;

    os << '\n';
    printer.print(ED);
    return true;
  }
  
  bool writeEnum(const EnumDecl *ED) {
    if (addImport(ED))
      return true;
    
    if (seenTypes[ED].first == EmissionState::Defined)
      return true;
    
    seenTypes[ED] = {EmissionState::Defined, true};
    printer.print(ED);

    ASTContext &ctx = M.getASTContext();

    SmallVector<ProtocolConformance *, 1> conformances;
    auto errorTypeProto = ctx.getProtocol(KnownProtocolKind::Error);
    if (ED->lookupConformance(&M, errorTypeProto, conformances)) {
      bool hasDomainCase = std::any_of(ED->getAllElements().begin(),
                                       ED->getAllElements().end(),
                                       [](const EnumElementDecl *elem) {
        return elem->getName().str() == "Domain";
      });
      if (!hasDomainCase) {
        os << "static NSString * _Nonnull const " << getNameForObjC(ED)
           << "Domain = @\"" << getErrorDomainStringForObjC(ED) << "\";\n";
      }
    }

    return true;
  }

  void writePrologue(raw_ostream &out) {
    out << "// Generated by " << version::getSwiftFullVersion(
      M.getASTContext().LangOpts.EffectiveLanguageVersion) << "\n"
           "#pragma clang diagnostic push\n"
           "#pragma clang diagnostic ignored \"-Wgcc-compat\"\n"
           "\n"
           "#if !defined(__has_include)\n"
           "# define __has_include(x) 0\n"
           "#endif\n"
           "#if !defined(__has_attribute)\n"
           "# define __has_attribute(x) 0\n"
           "#endif\n"
           "#if !defined(__has_feature)\n"
           "# define __has_feature(x) 0\n"
           "#endif\n"
           "#if !defined(__has_warning)\n"
           "# define __has_warning(x) 0\n"
           "#endif\n"
           "\n"
           "#if __has_include(<swift/objc-prologue.h>)\n"
           "# include <swift/objc-prologue.h>\n"
           "#endif\n"
           "\n"
           "#pragma clang diagnostic ignored \"-Wauto-import\"\n"
           "#include <Foundation/Foundation.h>\n"
           "#include <stdint.h>\n"
           "#include <stddef.h>\n"
           "#include <stdbool.h>\n"
           "\n"
           "#if !defined(SWIFT_TYPEDEFS)\n"
           "# define SWIFT_TYPEDEFS 1\n"
           "# if __has_include(<uchar.h>)\n"
           "#  include <uchar.h>\n"
           "# elif !defined(__cplusplus)\n"
           "typedef uint_least16_t char16_t;\n"
           "typedef uint_least32_t char32_t;\n"
           "# endif\n"
#define MAP_SIMD_TYPE(C_TYPE, SCALAR_TYPE, _) \
           "typedef " #SCALAR_TYPE " swift_" #C_TYPE "2"       \
           "  __attribute__((__ext_vector_type__(2)));\n" \
           "typedef " #SCALAR_TYPE " swift_" #C_TYPE "3"       \
           "  __attribute__((__ext_vector_type__(3)));\n" \
           "typedef " #SCALAR_TYPE " swift_" #C_TYPE "4"       \
           "  __attribute__((__ext_vector_type__(4)));\n"
#include "swift/ClangImporter/SIMDMappedTypes.def"
           "#endif\n"
           "\n"
           "#if !defined(SWIFT_PASTE)\n"
           "# define SWIFT_PASTE_HELPER(x, y) x##y\n"
           "# define SWIFT_PASTE(x, y) SWIFT_PASTE_HELPER(x, y)\n"
           "#endif"
           "\n"
           "#if !defined(SWIFT_METATYPE)\n"
           "# define SWIFT_METATYPE(X) Class\n"
           "#endif\n"
           "#if !defined(SWIFT_CLASS_PROPERTY)\n"
           "# if __has_feature(objc_class_property)\n"
           "#  define SWIFT_CLASS_PROPERTY(...) __VA_ARGS__\n"
           "# else\n"
           "#  define SWIFT_CLASS_PROPERTY(...)\n"
           "# endif\n"
           "#endif\n"
           "\n"
           "#if __has_attribute(objc_runtime_name)\n"
           "# define SWIFT_RUNTIME_NAME(X) "
             "__attribute__((objc_runtime_name(X)))\n"
           "#else\n"
           "# define SWIFT_RUNTIME_NAME(X)\n"
           "#endif\n"
           "#if __has_attribute(swift_name)\n"
           "# define SWIFT_COMPILE_NAME(X) "
             "__attribute__((swift_name(X)))\n"
           "#else\n"
           "# define SWIFT_COMPILE_NAME(X)\n"
           "#endif\n"
           "#if __has_attribute(objc_method_family)\n"
           "# define SWIFT_METHOD_FAMILY(X) "
             "__attribute__((objc_method_family(X)))\n"
           "#else\n"
           "# define SWIFT_METHOD_FAMILY(X)\n"
           "#endif\n"
           "#if __has_attribute(noescape)\n"
           "# define SWIFT_NOESCAPE __attribute__((noescape))\n"
           "#else\n"
           "# define SWIFT_NOESCAPE\n"
           "#endif\n"
           "#if __has_attribute(warn_unused_result)\n"
           "# define SWIFT_WARN_UNUSED_RESULT __attribute__((warn_unused_result))\n"
           "#else\n"
           "# define SWIFT_WARN_UNUSED_RESULT\n"
           "#endif\n"
           "#if __has_attribute(noreturn)\n"
           "# define SWIFT_NORETURN __attribute__((noreturn))\n"
           "#else\n"
           "# define SWIFT_NORETURN\n"
           "#endif\n"
           "#if !defined(SWIFT_CLASS_EXTRA)\n"
           "# define SWIFT_CLASS_EXTRA\n"
           "#endif\n"
           "#if !defined(SWIFT_PROTOCOL_EXTRA)\n"
           "# define SWIFT_PROTOCOL_EXTRA\n"
           "#endif\n"
           "#if !defined(SWIFT_ENUM_EXTRA)\n"
           "# define SWIFT_ENUM_EXTRA\n"
           "#endif\n"
           "#if !defined(SWIFT_CLASS)\n"
           "# if __has_attribute(objc_subclassing_restricted)\n"
           "#  define SWIFT_CLASS(SWIFT_NAME) SWIFT_RUNTIME_NAME(SWIFT_NAME) "
             "__attribute__((objc_subclassing_restricted)) "
             "SWIFT_CLASS_EXTRA\n"
           "#  define SWIFT_CLASS_NAMED(SWIFT_NAME) "
             "__attribute__((objc_subclassing_restricted)) "
             "SWIFT_COMPILE_NAME(SWIFT_NAME) "
             "SWIFT_CLASS_EXTRA\n"
           "# else\n"
           "#  define SWIFT_CLASS(SWIFT_NAME) SWIFT_RUNTIME_NAME(SWIFT_NAME) "
             "SWIFT_CLASS_EXTRA\n"
           "#  define SWIFT_CLASS_NAMED(SWIFT_NAME) "
             "SWIFT_COMPILE_NAME(SWIFT_NAME) "
             "SWIFT_CLASS_EXTRA\n"
           "# endif\n"
           "#endif\n"
           "#if !defined(SWIFT_RESILIENT_CLASS)\n"
           "# if __has_attribute(objc_class_stub)\n"
           "#  define SWIFT_RESILIENT_CLASS(SWIFT_NAME) SWIFT_CLASS(SWIFT_NAME) "
             "__attribute__((objc_class_stub))\n"
           "#  define SWIFT_RESILIENT_CLASS_NAMED(SWIFT_NAME) "
             "__attribute__((objc_class_stub)) "
             "SWIFT_CLASS_NAMED(SWIFT_NAME)\n"
           "# else\n"
           "#  define SWIFT_RESILIENT_CLASS(SWIFT_NAME) "
             "SWIFT_CLASS(SWIFT_NAME)\n"
           "#  define SWIFT_RESILIENT_CLASS_NAMED(SWIFT_NAME) "
             "SWIFT_CLASS_NAMED(SWIFT_NAME)\n"
           "# endif\n"
           "#endif\n"
           "\n"
           "#if !defined(SWIFT_PROTOCOL)\n"
           "# define SWIFT_PROTOCOL(SWIFT_NAME) SWIFT_RUNTIME_NAME(SWIFT_NAME) "
             "SWIFT_PROTOCOL_EXTRA\n"
           "# define SWIFT_PROTOCOL_NAMED(SWIFT_NAME) "
             "SWIFT_COMPILE_NAME(SWIFT_NAME) "
             "SWIFT_PROTOCOL_EXTRA\n"
           "#endif\n"
           "\n"
           "#if !defined(SWIFT_EXTENSION)\n"
           "# define SWIFT_EXTENSION(M) SWIFT_PASTE(M##_Swift_, __LINE__)\n"
           "#endif\n"
           "\n"
           "#if !defined(OBJC_DESIGNATED_INITIALIZER)\n"
           "# if __has_attribute(objc_designated_initializer)\n"
           "#  define OBJC_DESIGNATED_INITIALIZER "
             "__attribute__((objc_designated_initializer))\n"
           "# else\n"
           "#  define OBJC_DESIGNATED_INITIALIZER\n"
           "# endif\n"
           "#endif\n"
           "#if !defined(SWIFT_ENUM_ATTR)\n"
           "# if defined(__has_attribute) && "
             "__has_attribute(enum_extensibility)\n"
           "#  define SWIFT_ENUM_ATTR(_extensibility) "
             "__attribute__((enum_extensibility(_extensibility)))\n"
           "# else\n"
           "#  define SWIFT_ENUM_ATTR(_extensibility)\n"
           "# endif\n"
           "#endif\n"
           "#if !defined(SWIFT_ENUM)\n"
           "# define SWIFT_ENUM(_type, _name, _extensibility) "
             "enum _name : _type _name; "
             "enum SWIFT_ENUM_ATTR(_extensibility) SWIFT_ENUM_EXTRA "
             "_name : _type\n"
           "# if __has_feature(generalized_swift_name)\n"
           "#  define SWIFT_ENUM_NAMED(_type, _name, SWIFT_NAME, "
             "_extensibility) "
             "enum _name : _type _name SWIFT_COMPILE_NAME(SWIFT_NAME); "
             "enum SWIFT_COMPILE_NAME(SWIFT_NAME) "
             "SWIFT_ENUM_ATTR(_extensibility) SWIFT_ENUM_EXTRA _name : _type\n"
           "# else\n"
           "#  define SWIFT_ENUM_NAMED(_type, _name, SWIFT_NAME, "
             "_extensibility) SWIFT_ENUM(_type, _name, _extensibility)\n"
           "# endif\n"
           "#endif\n"
           "#if !defined(SWIFT_UNAVAILABLE)\n"
           "# define SWIFT_UNAVAILABLE __attribute__((unavailable))\n"
           "#endif\n"
           "#if !defined(SWIFT_UNAVAILABLE_MSG)\n"
           "# define SWIFT_UNAVAILABLE_MSG(msg) __attribute__((unavailable(msg)))\n"
           "#endif\n"
           "#if !defined(SWIFT_AVAILABILITY)\n"
           "# define SWIFT_AVAILABILITY(plat, ...) __attribute__((availability(plat, __VA_ARGS__)))\n"
           "#endif\n"
           "#if !defined(SWIFT_WEAK_IMPORT)\n"
           "# define SWIFT_WEAK_IMPORT __attribute__((weak_import))\n"
           "#endif\n"
           "#if !defined(SWIFT_DEPRECATED)\n"
           "# define SWIFT_DEPRECATED __attribute__((deprecated))\n"
           "#endif\n"
           "#if !defined(SWIFT_DEPRECATED_MSG)\n"
           "# define SWIFT_DEPRECATED_MSG(...) __attribute__((deprecated(__VA_ARGS__)))\n"
           "#endif\n"
           "#if __has_feature(attribute_diagnose_if_objc)\n"
           "# define SWIFT_DEPRECATED_OBJC(Msg) __attribute__((diagnose_if(1, Msg, \"warning\")))\n"
           "#else\n"
           "# define SWIFT_DEPRECATED_OBJC(Msg) SWIFT_DEPRECATED_MSG(Msg)\n"
           "#endif\n"
           "#if !defined(IBSegueAction)\n"
           "# define IBSegueAction\n"
           "#endif\n"
           ;
    static_assert(SWIFT_MAX_IMPORTED_SIMD_ELEMENTS == 4,
                "need to add SIMD typedefs here if max elements is increased");
  }

  bool isUnderlyingModule(ModuleDecl *import) {
    if (bridgingHeader.empty())
      return import != &M && import->getName() == M.getName();

    auto importer =
      static_cast<ClangImporter *>(import->getASTContext()
                                     .getClangModuleLoader());
    return import == importer->getImportedHeaderModule();
  }

  static int compareImportModulesByName(const ImportModuleTy *left,
                                        const ImportModuleTy *right) {
    auto *leftSwiftModule = left->dyn_cast<ModuleDecl *>();
    auto *rightSwiftModule = right->dyn_cast<ModuleDecl *>();

    if (leftSwiftModule && !rightSwiftModule)
      return -compareImportModulesByName(right, left);

    if (leftSwiftModule && rightSwiftModule)
      return leftSwiftModule->getName().compare(rightSwiftModule->getName());

    auto *leftClangModule = left->get<const clang::Module *>();
    assert(leftClangModule->isSubModule() &&
           "top-level modules should use a normal swift::ModuleDecl");
    if (rightSwiftModule) {
      // Because the Clang module is a submodule, its full name will never be
      // equal to a Swift module's name, even if the top-level name is the same;
      // it will always come before or after.
      if (leftClangModule->getTopLevelModuleName() <
          rightSwiftModule->getName().str()) {
        return -1;
      }
      return 1;
    }

    auto *rightClangModule = right->get<const clang::Module *>();
    assert(rightClangModule->isSubModule() &&
           "top-level modules should use a normal swift::ModuleDecl");

    SmallVector<StringRef, 8> leftReversePath(
        ModuleDecl::ReverseFullNameIterator(leftClangModule), {});
    SmallVector<StringRef, 8> rightReversePath(
        ModuleDecl::ReverseFullNameIterator(rightClangModule), {});

    assert(leftReversePath != rightReversePath &&
           "distinct Clang modules should not have the same full name");
    if (std::lexicographical_compare(leftReversePath.rbegin(),
                                     leftReversePath.rend(),
                                     rightReversePath.rbegin(),
                                     rightReversePath.rend())) {
      return -1;
    }
    return 1;
  }

  void writeImports(raw_ostream &out) {
    out << "#if __has_feature(modules)\n";

    out << "#if __has_warning(\"-Watimport-in-framework-header\")\n"
        << "#pragma clang diagnostic ignored \"-Watimport-in-framework-header\"\n"
        << "#endif\n";

    // Sort alphabetically for determinism and consistency.
    SmallVector<ImportModuleTy, 8> sortedImports{imports.begin(),
                                                 imports.end()};
    llvm::array_pod_sort(sortedImports.begin(), sortedImports.end(),
                         &compareImportModulesByName);

    // Track printed names to handle overlay modules.
    llvm::SmallPtrSet<Identifier, 8> seenImports;
    bool includeUnderlying = false;
    for (auto import : sortedImports) {
      if (auto *swiftModule = import.dyn_cast<ModuleDecl *>()) {
        auto Name = swiftModule->getName();
        if (isUnderlyingModule(swiftModule)) {
          includeUnderlying = true;
          continue;
        }
        if (seenImports.insert(Name).second)
          out << "@import " << Name.str() << ";\n";
      } else {
        const auto *clangModule = import.get<const clang::Module *>();
        assert(clangModule->isSubModule() &&
               "top-level modules should use a normal swift::ModuleDecl");
        out << "@import ";
        ModuleDecl::ReverseFullNameIterator(clangModule).printForward(out);
        out << ";\n";
      }
    }

    out << "#endif\n\n";

    if (includeUnderlying) {
      if (bridgingHeader.empty())
        out << "#import <" << M.getName().str() << '/' << M.getName().str()
            << ".h>\n\n";
      else
        out << "#import \"" << bridgingHeader << "\"\n\n";
    }
  }

  bool writeToStream(raw_ostream &out) {
    SmallVector<Decl *, 64> decls;
    M.getTopLevelDecls(decls);

    auto newEnd = std::remove_if(decls.begin(), decls.end(),
                                 [this](const Decl *D) -> bool {
      if (auto VD = dyn_cast<ValueDecl>(D))
        return !printer.shouldInclude(VD);

      if (auto ED = dyn_cast<ExtensionDecl>(D)) {
        auto baseClass = ED->getSelfClassDecl();
        return !baseClass || !printer.shouldInclude(baseClass) ||
               baseClass->isForeign();
      }
      return true;
    });
    decls.erase(newEnd, decls.end());

    // REVERSE sort the decls, since we are going to copy them onto a stack.
    llvm::array_pod_sort(decls.begin(), decls.end(),
                         [](Decl * const *lhs, Decl * const *rhs) -> int {
      enum : int {
        Ascending = -1,
        Equivalent = 0,
        Descending = 1,
      };

      assert(*lhs != *rhs && "duplicate top-level decl");

      auto getSortName = [](const Decl *D) -> StringRef {
        if (auto VD = dyn_cast<ValueDecl>(D))
          return VD->getBaseName().userFacingName();

        if (auto ED = dyn_cast<ExtensionDecl>(D)) {
          auto baseClass = ED->getSelfClassDecl();
          return baseClass->getName().str();
        }
        llvm_unreachable("unknown top-level ObjC decl");
      };

      // Sort by names.
      int result = getSortName(*rhs).compare(getSortName(*lhs));
      if (result != 0)
        return result;

      // Prefer value decls to extensions.
      assert(!(isa<ValueDecl>(*lhs) && isa<ValueDecl>(*rhs)));
      if (isa<ValueDecl>(*lhs) && !isa<ValueDecl>(*rhs))
        return Descending;
      if (!isa<ValueDecl>(*lhs) && isa<ValueDecl>(*rhs))
        return Ascending;

      // Break ties in extensions by putting smaller extensions last (in reverse
      // order).
      // FIXME: This will end up taking linear time.
      auto lhsMembers = cast<ExtensionDecl>(*lhs)->getMembers();
      auto rhsMembers = cast<ExtensionDecl>(*rhs)->getMembers();
      unsigned numLHSMembers = std::distance(lhsMembers.begin(),
                                             lhsMembers.end());
      unsigned numRHSMembers = std::distance(rhsMembers.begin(),
                                             rhsMembers.end());
      if (numLHSMembers != numRHSMembers)
        return numLHSMembers < numRHSMembers ? Descending : Ascending;

      // Or the extension with fewer protocols.
      auto lhsProtos = cast<ExtensionDecl>(*lhs)->getLocalProtocols();
      auto rhsProtos = cast<ExtensionDecl>(*rhs)->getLocalProtocols();
      if (lhsProtos.size() != rhsProtos.size())
        return lhsProtos.size() < rhsProtos.size() ? Descending : Ascending;

      // If that fails, arbitrarily pick the extension whose protocols are
      // alphabetically first.
      auto mismatch =
        std::mismatch(lhsProtos.begin(), lhsProtos.end(), rhsProtos.begin(),
                      [] (const ProtocolDecl *nextLHSProto,
                          const ProtocolDecl *nextRHSProto) {
        return nextLHSProto->getName() != nextRHSProto->getName();
      });
      if (mismatch.first == lhsProtos.end())
        return Equivalent;
      StringRef lhsProtoName = (*mismatch.first)->getName().str();
      return lhsProtoName.compare((*mismatch.second)->getName().str());
    });

    assert(declsToWrite.empty());
    declsToWrite.assign(decls.begin(), decls.end());

    while (!declsToWrite.empty()) {
      const Decl *D = declsToWrite.back();
      bool success = true;

      if (isa<ValueDecl>(D)) {
        if (auto CD = dyn_cast<ClassDecl>(D))
          success = writeClass(CD);
        else if (auto PD = dyn_cast<ProtocolDecl>(D))
          success = writeProtocol(PD);
        else if (auto ED = dyn_cast<EnumDecl>(D))
          success = writeEnum(ED);
        else if (auto ED = dyn_cast<FuncDecl>(D))
          success = writeFunc(ED);
        else
          llvm_unreachable("unknown top-level ObjC value decl");

      } else if (auto ED = dyn_cast<ExtensionDecl>(D)) {
        success = writeExtension(ED);

      } else {
        llvm_unreachable("unknown top-level ObjC decl");
      }

      if (success) {
        assert(declsToWrite.back() == D);
        os << "\n";
        declsToWrite.pop_back();
      }
    }

    if (!delayedMembers.empty()) {
      auto groupBegin = delayedMembers.begin();
      for (auto i = groupBegin, e = delayedMembers.end(); i != e; ++i) {
        if ((*i)->getDeclContext() != (*groupBegin)->getDeclContext()) {
          printer.printAdHocCategory(make_range(groupBegin, i));
          groupBegin = i;
        }
      }
      printer.printAdHocCategory(make_range(groupBegin, delayedMembers.end()));
    }

    writePrologue(out);
    writeImports(out);
    out <<
        "#pragma clang diagnostic ignored \"-Wproperty-attribute-mismatch\"\n"
        "#pragma clang diagnostic ignored \"-Wduplicate-method-arg\"\n"
        "#if __has_warning(\"-Wpragma-clang-attribute\")\n"
        "# pragma clang diagnostic ignored \"-Wpragma-clang-attribute\"\n"
        "#endif\n"
        "#pragma clang diagnostic ignored \"-Wunknown-pragmas\"\n"
        "#pragma clang diagnostic ignored \"-Wnullability\"\n"
        "\n"
        "#if __has_attribute(external_source_symbol)\n"
        "# pragma push_macro(\"any\")\n"
        "# undef any\n"
        "# pragma clang attribute push("
          "__attribute__((external_source_symbol(language=\"Swift\", "
            "defined_in=\"" << M.getNameStr() << "\",generated_declaration))), "
          "apply_to=any(function,enum,objc_interface,objc_category,"
             "objc_protocol))\n"
        "# pragma pop_macro(\"any\")\n"
        "#endif\n\n"
      << os.str()
      << "#if __has_attribute(external_source_symbol)\n"
         "# pragma clang attribute pop\n"
         "#endif\n"
         "#pragma clang diagnostic pop\n";
    return false;
  }
};
} // end anonymous namespace

bool swift::printAsObjC(llvm::raw_ostream &os, ModuleDecl *M,
                        StringRef bridgingHeader,
                        AccessLevel minRequiredAccess) {
  llvm::PrettyStackTraceString trace("While generating Objective-C header");
  return ModuleWriter(*M, bridgingHeader, minRequiredAccess).writeToStream(os);
}
