//===------- ModuleInterfaceSupport.cpp - swiftinterface files ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/FileSystem.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/ModuleInterfaceSupport.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/Serialization/SerializationOptions.h"
#include "swift/Serialization/Validation.h"
#include "clang/Basic/Module.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Regex.h"
#include "llvm/Support/StringSaver.h"

using namespace swift;

version::Version swift::InterfaceFormatVersion({1, 0});

/// Diagnose any scoped imports in \p imports, i.e. those with a non-empty
/// access path. These are not yet supported by module interfaces, since the
/// information about the declaration kind is not preserved through the binary
/// serialization that happens as an intermediate step in non-whole-module
/// builds.
///
/// These come from declarations like `import class FooKit.MainFooController`.
static void diagnoseScopedImports(DiagnosticEngine &diags,
                                  ArrayRef<ModuleDecl::ImportedModule> imports){
  for (const ModuleDecl::ImportedModule &importPair : imports) {
    if (importPair.first.empty())
      continue;
    diags.diagnose(importPair.first.front().Loc,
                   diag::module_interface_scoped_import_unsupported);
  }
}

/// Prints to \p out a comment containing a format version number, tool version
/// string as well as any relevant command-line flags in \p Opts used to
/// construct \p M.
static void printToolVersionAndFlagsComment(raw_ostream &out,
                                            ModuleInterfaceOptions const &Opts,
                                            ModuleDecl *M) {
  auto &Ctx = M->getASTContext();
  auto ToolsVersion =
      getSwiftInterfaceCompilerVersionForCurrentCompiler(Ctx);
  out << "// " SWIFT_INTERFACE_FORMAT_VERSION_KEY ": "
      << InterfaceFormatVersion << "\n";
  out << "// " SWIFT_COMPILER_VERSION_KEY ": "
      << ToolsVersion << "\n";
  out << "// " SWIFT_MODULE_FLAGS_KEY ": "
      << Opts.Flags << "\n";
}

std::string
swift::getSwiftInterfaceCompilerVersionForCurrentCompiler(ASTContext &ctx) {
  return swift::version::getSwiftFullVersion(
             ctx.LangOpts.EffectiveLanguageVersion);
}

llvm::Regex swift::getSwiftInterfaceFormatVersionRegex() {
  return llvm::Regex("^// " SWIFT_INTERFACE_FORMAT_VERSION_KEY
                     ": ([0-9\\.]+)$", llvm::Regex::Newline);
}

llvm::Regex swift::getSwiftInterfaceModuleFlagsRegex() {
  return llvm::Regex("^// " SWIFT_MODULE_FLAGS_KEY ":(.*)$",
                     llvm::Regex::Newline);
}

llvm::Regex swift::getSwiftInterfaceCompilerVersionRegex() {
  return llvm::Regex("^// " SWIFT_COMPILER_VERSION_KEY
                     ": (.+)$", llvm::Regex::Newline);
}

/// Prints the imported modules in \p M to \p out in the form of \c import
/// source declarations.
static void printImports(raw_ostream &out,
                         ModuleInterfaceOptions const &Opts,
                         ModuleDecl *M) {
  // FIXME: This is very similar to what's in Serializer::writeInputBlock, but
  // it's not obvious what higher-level optimization would be factored out here.
  ModuleDecl::ImportFilter allImportFilter;
  allImportFilter |= ModuleDecl::ImportFilterKind::Public;
  allImportFilter |= ModuleDecl::ImportFilterKind::Private;
  allImportFilter |= ModuleDecl::ImportFilterKind::SPIAccessControl;

  // With -experimental-spi-imports:
  // When printing the private swiftinterface file, print implementation-only
  // imports only if they are also SPI. First, list all implementation-only
  // imports and filter them later.
  llvm::SmallSet<ModuleDecl::ImportedModule, 4,
                 ModuleDecl::OrderImportedModules> ioiImportSet;
  if (Opts.PrintSPIs && Opts.ExperimentalSPIImports) {
    allImportFilter |= ModuleDecl::ImportFilterKind::ImplementationOnly;

    SmallVector<ModuleDecl::ImportedModule, 4> ioiImport;
    M->getImportedModules(ioiImport,
                          ModuleDecl::ImportFilterKind::ImplementationOnly);
    ioiImportSet.insert(ioiImport.begin(), ioiImport.end());
  }

  SmallVector<ModuleDecl::ImportedModule, 8> allImports;
  M->getImportedModules(allImports, allImportFilter);
  ModuleDecl::removeDuplicateImports(allImports);
  diagnoseScopedImports(M->getASTContext().Diags, allImports);

  // Collect the public imports as a subset so that we can mark them with
  // '@_exported'.
  SmallVector<ModuleDecl::ImportedModule, 8> publicImports;
  M->getImportedModules(publicImports, ModuleDecl::ImportFilterKind::Public);
  llvm::SmallSet<ModuleDecl::ImportedModule, 8,
                 ModuleDecl::OrderImportedModules> publicImportSet;
  publicImportSet.insert(publicImports.begin(), publicImports.end());

  for (auto import : allImports) {
    auto importedModule = import.second;
    if (importedModule->isOnoneSupportModule() ||
        importedModule->isBuiltinModule()) {
      continue;
    }

    llvm::SmallVector<Identifier, 4> spis;
    M->lookupImportedSPIGroups(importedModule, spis);

    // Only print implementation-only imports which have an SPI import.
    if (ioiImportSet.count(import)) {
      if (spis.empty())
        continue;
      out << "@_implementationOnly ";
    }

    if (publicImportSet.count(import))
      out << "@_exported ";

    // SPI attribute on imports
    if (Opts.PrintSPIs) {
      for (auto spiName : spis)
        out << "@_spi(" << spiName << ") ";
    }

    out << "import ";
    importedModule->getReverseFullModuleName().printForward(out);

    // Write the access path we should be honoring but aren't.
    // (See diagnoseScopedImports above.)
    if (!import.first.empty()) {
      out << "/*";
      for (const auto &accessPathElem : import.first)
        out << "." << accessPathElem.Item;
      out << "*/";
    }

    out << "\n";
  }
}

// FIXME: Copied from ASTPrinter.cpp...
static bool isPublicOrUsableFromInline(const ValueDecl *VD) {
  AccessScope scope =
      VD->getFormalAccessScope(/*useDC*/nullptr,
                               /*treatUsableFromInlineAsPublic*/true);
  return scope.isPublic();
}

static bool isPublicOrUsableFromInline(Type ty) {
  // Note the double negative here: we're looking for any referenced decls that
  // are *not* public-or-usableFromInline.
  return !ty.findIf([](Type typePart) -> bool {
    // FIXME: If we have an internal typealias for a non-internal type, we ought
    // to be able to print it by desugaring.
    if (auto *aliasTy = dyn_cast<TypeAliasType>(typePart.getPointer()))
      return !isPublicOrUsableFromInline(aliasTy->getDecl());
    if (auto *nominal = typePart->getAnyNominal())
      return !isPublicOrUsableFromInline(nominal);
    return false;
  });
}

namespace {
/// Collects protocols that are conformed to by a particular nominal. Since
/// ASTPrinter will only print the public ones, the non-public ones get left by
/// the wayside. This is a problem when a non-public protocol inherits from a
/// public protocol; the generated module interface still needs to make that
/// dependency public.
///
/// The solution implemented here is to generate synthetic extensions that
/// declare the extra conformances. This isn't perfect (it loses the sugared
/// spelling of the protocol type, as well as the locality in the file), but it
/// does work.
class InheritedProtocolCollector {
  static const StringLiteral DummyProtocolName;

  using AvailableAttrList = TinyPtrVector<const AvailableAttr *>;
  using ProtocolAndAvailability =
    std::pair<ProtocolDecl *, AvailableAttrList>;

  /// Protocols that will be included by the ASTPrinter without any extra work.
  SmallVector<ProtocolDecl *, 8> IncludedProtocols;
  /// Protocols that will not be printed by the ASTPrinter, along with the
  /// availability they were declared with.
  SmallVector<ProtocolAndAvailability, 8> ExtraProtocols;
  /// Protocols that can be printed, but whose conformances are constrained with
  /// something that \e can't be printed.
  SmallVector<const ProtocolType *, 8> ConditionalConformanceProtocols;

  /// Helper to extract the `@available` attributes on a decl.
  static AvailableAttrList
  getAvailabilityAttrs(const Decl *D, Optional<AvailableAttrList> &cache) {
    if (cache.hasValue())
      return cache.getValue();

    cache.emplace();
    while (D) {
      for (auto *nextAttr : D->getAttrs().getAttributes<AvailableAttr>()) {
        // FIXME: This is just approximating the effects of nested availability
        // attributes for the same platform; formally they'd need to be merged.
        bool alreadyHasMoreSpecificAttrForThisPlatform =
            llvm::any_of(*cache, [nextAttr](const AvailableAttr *existingAttr) {
          return existingAttr->Platform == nextAttr->Platform;
        });
        if (alreadyHasMoreSpecificAttrForThisPlatform)
          continue;
        cache->push_back(nextAttr);
      }
      D = D->getDeclContext()->getAsDecl();
    }

    return cache.getValue();
  }

  /// For each type in \p directlyInherited, classify the protocols it refers to
  /// as included for printing or not, and record them in the appropriate
  /// vectors.
  void recordProtocols(ArrayRef<TypeLoc> directlyInherited, const Decl *D) {
    Optional<AvailableAttrList> availableAttrs;

    for (TypeLoc inherited : directlyInherited) {
      Type inheritedTy = inherited.getType();
      if (!inheritedTy || !inheritedTy->isExistentialType())
        continue;

      bool canPrintNormally = isPublicOrUsableFromInline(inheritedTy);
      ExistentialLayout layout = inheritedTy->getExistentialLayout();
      for (ProtocolType *protoTy : layout.getProtocols()) {
        if (canPrintNormally)
          IncludedProtocols.push_back(protoTy->getDecl());
        else
          ExtraProtocols.push_back({protoTy->getDecl(),
                                    getAvailabilityAttrs(D, availableAttrs)});
      }
      // FIXME: This ignores layout constraints, but currently we don't support
      // any of those besides 'AnyObject'.
    }

    // Check for synthesized protocols, like Hashable on enums.
    if (auto *nominal = dyn_cast<NominalTypeDecl>(D)) {
      SmallVector<ProtocolConformance *, 4> localConformances =
          nominal->getLocalConformances(ConformanceLookupKind::NonInherited);

      for (auto *conf : localConformances) {
        if (conf->getSourceKind() != ConformanceEntryKind::Synthesized)
          continue;
        ExtraProtocols.push_back({conf->getProtocol(),
                                  getAvailabilityAttrs(D, availableAttrs)});
      }
    }
  }

  /// For each type directly inherited by \p extension, record any protocols
  /// that we would have printed in ConditionalConformanceProtocols.
  void recordConditionalConformances(const ExtensionDecl *extension) {
    for (TypeLoc inherited : extension->getInherited()) {
      Type inheritedTy = inherited.getType();
      if (!inheritedTy || !inheritedTy->isExistentialType())
        continue;

      ExistentialLayout layout = inheritedTy->getExistentialLayout();
      for (ProtocolType *protoTy : layout.getProtocols()) {
        if (!isPublicOrUsableFromInline(protoTy))
          continue;
        ConditionalConformanceProtocols.push_back(protoTy);
      }
      // FIXME: This ignores layout constraints, but currently we don't support
      // any of those besides 'AnyObject'.
    }
  }

public:
  using PerTypeMap = llvm::MapVector<const NominalTypeDecl *,
                                     InheritedProtocolCollector>;

  /// Given that we're about to print \p D, record its protocols in \p map.
  ///
  /// \sa recordProtocols
  static void collectProtocols(PerTypeMap &map, const Decl *D) {
    ArrayRef<TypeLoc> directlyInherited;
    const NominalTypeDecl *nominal;
    const IterableDeclContext *memberContext;

    auto shouldInclude = [](const ExtensionDecl *extension) {
      if (extension->isConstrainedExtension()) {
        // Conditional conformances never apply to inherited protocols, nor
        // can they provide unconditional conformances that might be used in
        // other extensions.
        return false;
      }
      return true;
    };
    if ((nominal = dyn_cast<NominalTypeDecl>(D))) {
      directlyInherited = nominal->getInherited();
      memberContext = nominal;

    } else if (auto *extension = dyn_cast<ExtensionDecl>(D)) {
      if (!shouldInclude(extension)) {
        return;
      }
      nominal = extension->getExtendedNominal();
      directlyInherited = extension->getInherited();
      memberContext = extension;
    } else {
      return;
    }

    if (!isPublicOrUsableFromInline(nominal))
      return;

    map[nominal].recordProtocols(directlyInherited, D);
    // Collect protocols inherited from super classes
    if (auto *CD = dyn_cast<ClassDecl>(D)) {
      for (auto *SD = CD->getSuperclassDecl(); SD;
           SD = SD->getSuperclassDecl()) {
        map[nominal].recordProtocols(SD->getInherited(), SD);
        for (auto *Ext: SD->getExtensions()) {
          if (shouldInclude(Ext)) {
            map[nominal].recordProtocols(Ext->getInherited(), Ext);
          }
        }
      }
    }

    // Recurse to find any nested types.
    for (const Decl *member : memberContext->getMembers())
      collectProtocols(map, member);
  }

  /// If \p D is an extension providing conditional conformances, record those
  /// in \p map.
  ///
  /// \sa recordConditionalConformances
  static void collectSkippedConditionalConformances(
                                            PerTypeMap &map,
                                            const Decl *D,
                                            const PrintOptions &printOptions) {
    auto *extension = dyn_cast<ExtensionDecl>(D);
    if (!extension || !extension->isConstrainedExtension())
      return;

    // Skip SPI extensions in the public interface.
    if (!printOptions.PrintSPIs && extension->isSPI())
      return;

    const NominalTypeDecl *nominal = extension->getExtendedNominal();
    if (!isPublicOrUsableFromInline(nominal))
      return;

    map[nominal].recordConditionalConformances(extension);
    // No recursion here because extensions are never nested.
  }

  /// Returns true if the conformance of \p nominal to \p proto is declared in
  /// module \p M.
  static bool conformanceDeclaredInModule(ModuleDecl *M,
                                          const NominalTypeDecl *nominal,
                                          ProtocolDecl *proto) {
    SmallVector<ProtocolConformance *, 4> conformances;
    nominal->lookupConformance(M, proto, conformances);
    return llvm::all_of(conformances,
                        [M](const ProtocolConformance *conformance) -> bool {
      return M == conformance->getDeclContext()->getParentModule();
    });
  }

  /// If there were any public protocols that need to be printed (i.e. they
  /// weren't conformed to explicitly or inherited by another printed protocol),
  /// do so now by printing a dummy extension on \p nominal to \p out.
  void
  printSynthesizedExtensionIfNeeded(raw_ostream &out,
                                    const PrintOptions &printOptions,
                                    ModuleDecl *M,
                                    const NominalTypeDecl *nominal) const {
    if (ExtraProtocols.empty())
      return;

    if (!printOptions.shouldPrint(nominal))
      return;

    SmallPtrSet<ProtocolDecl *, 16> handledProtocols;

    // First record all protocols that have already been handled.
    for (ProtocolDecl *proto : IncludedProtocols) {
      proto->walkInheritedProtocols(
          [&handledProtocols](ProtocolDecl *inherited) -> TypeWalker::Action {
        handledProtocols.insert(inherited);
        return TypeWalker::Action::Continue;
      });
    }

    // Then walk the remaining ones, and see what we need to print.
    // Note: We could do this in one pass, but the logic is easier to
    // understand if we build up the list and then print it, even if it takes
    // a bit more memory.
    // FIXME: This will pick the availability attributes from the first sight
    // of a protocol rather than the maximally available case.
    SmallVector<ProtocolAndAvailability, 16> protocolsToPrint;
    for (const auto &protoAndAvailability : ExtraProtocols) {
      protoAndAvailability.first->walkInheritedProtocols(
          [&](ProtocolDecl *inherited) -> TypeWalker::Action {
        if (!handledProtocols.insert(inherited).second)
          return TypeWalker::Action::SkipChildren;

        if (isPublicOrUsableFromInline(inherited) &&
            conformanceDeclaredInModule(M, nominal, inherited)) {
          protocolsToPrint.push_back({inherited, protoAndAvailability.second});
          return TypeWalker::Action::SkipChildren;
        }

        return TypeWalker::Action::Continue;
      });
    }
    if (protocolsToPrint.empty())
      return;

    for (const auto &protoAndAvailability : protocolsToPrint) {
      StreamPrinter printer(out);
      // FIXME: Shouldn't this be an implicit conversion?
      TinyPtrVector<const DeclAttribute *> attrs;
      attrs.insert(attrs.end(), protoAndAvailability.second.begin(),
                   protoAndAvailability.second.end());
      DeclAttributes::print(printer, printOptions, attrs);

      printer << "extension ";
      nominal->getDeclaredType().print(printer, printOptions);
      printer << " : ";

      ProtocolDecl *proto = protoAndAvailability.first;
      proto->getDeclaredType()->print(printer, printOptions);

      printer << " {}\n";
    }
  }

  /// If there were any conditional conformances that couldn't be printed,
  /// make a dummy extension that conforms to all of them, constrained by a
  /// fake protocol.
  bool printInaccessibleConformanceExtensionIfNeeded(
      raw_ostream &out, const PrintOptions &printOptions,
      const NominalTypeDecl *nominal) const {
    if (ConditionalConformanceProtocols.empty())
      return false;
    assert(nominal->isGenericContext());

    out << "@available(*, unavailable)\nextension ";
    nominal->getDeclaredType().print(out, printOptions);
    out << " : ";
    llvm::interleave(
        ConditionalConformanceProtocols,
        [&out, &printOptions](const ProtocolType *protoTy) {
          protoTy->print(out, printOptions);
        },
        [&out] { out << ", "; });
    out << " where "
        << nominal->getGenericSignature()->getGenericParams().front()->getName()
        << " : " << DummyProtocolName << " {}\n";
    return true;
  }

  /// Print a fake protocol declaration for use by
  /// #printInaccessibleConformanceExtensionIfNeeded.
  static void printDummyProtocolDeclaration(raw_ostream &out) {
    out << "\n@usableFromInline\ninternal protocol " << DummyProtocolName
        << " {}\n";
  }
};

const StringLiteral InheritedProtocolCollector::DummyProtocolName =
    "_ConstraintThatIsNotPartOfTheAPIOfThisLibrary";
} // end anonymous namespace

bool swift::emitSwiftInterface(raw_ostream &out,
                               ModuleInterfaceOptions const &Opts,
                               ModuleDecl *M) {
  assert(M);

  printToolVersionAndFlagsComment(out, Opts, M);
  printImports(out, Opts, M);

  const PrintOptions printOptions = PrintOptions::printSwiftInterfaceFile(
      Opts.PreserveTypesAsWritten, Opts.PrintFullConvention, Opts.PrintSPIs);
  InheritedProtocolCollector::PerTypeMap inheritedProtocolMap;

  SmallVector<Decl *, 16> topLevelDecls;
  M->getTopLevelDecls(topLevelDecls);
  for (const Decl *D : topLevelDecls) {
    InheritedProtocolCollector::collectProtocols(inheritedProtocolMap, D);

    if (!D->shouldPrintInContext(printOptions) ||
        !printOptions.shouldPrint(D)) {

      InheritedProtocolCollector::collectSkippedConditionalConformances(
          inheritedProtocolMap, D, printOptions);
      continue;
    }

    D->print(out, printOptions);
    out << "\n";
  }

  // Print dummy extensions for any protocols that were indirectly conformed to.
  bool needDummyProtocolDeclaration = false;
  for (const auto &nominalAndCollector : inheritedProtocolMap) {
    const NominalTypeDecl *nominal = nominalAndCollector.first;
    const InheritedProtocolCollector &collector = nominalAndCollector.second;
    collector.printSynthesizedExtensionIfNeeded(out, printOptions, M, nominal);
    needDummyProtocolDeclaration |=
        collector.printInaccessibleConformanceExtensionIfNeeded(out,
                                                                printOptions,
                                                                nominal);
  }
  if (needDummyProtocolDeclaration)
    InheritedProtocolCollector::printDummyProtocolDeclaration(out);

  return false;
}
