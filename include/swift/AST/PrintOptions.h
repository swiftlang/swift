//===--- PrintOptions.h - AST printing options ------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_PRINTOPTIONS_H
#define SWIFT_AST_PRINTOPTIONS_H

#include "swift/Basic/STLExtras.h"
#include "swift/AST/AttrKind.h"
#include "swift/AST/Identifier.h"
#include "llvm/ADT/Optional.h"
#include <limits.h>
#include <vector>

namespace swift {
class GenericEnvironment;
class CanType;
class Decl;
class ValueDecl;
class ExtensionDecl;
class NominalTypeDecl;
class TypeBase;
class DeclContext;
class Type;
class ModuleDecl;
enum DeclAttrKind : unsigned;
class SynthesizedExtensionAnalyzer;
struct PrintOptions;

/// Necessary information for archetype transformation during printing.
struct TypeTransformContext {
  TypeBase *BaseType;
  NominalTypeDecl *Nominal = nullptr;

  explicit TypeTransformContext(Type T);
  explicit TypeTransformContext(NominalTypeDecl* NTD);

  Type getBaseType() const;
  NominalTypeDecl *getNominal() const;

  bool isPrintingSynthesizedExtension() const;
};

typedef std::pair<ExtensionDecl*, bool> ExtensionAndIsSynthesized;
typedef llvm::function_ref<void(ArrayRef<ExtensionAndIsSynthesized>)>
  ExtensionGroupOperation;

class SynthesizedExtensionAnalyzer {
  struct Implementation;
  Implementation &Impl;
public:
  SynthesizedExtensionAnalyzer(NominalTypeDecl *Target,
                               PrintOptions Options,
                               bool IncludeUnconditional = true);
  ~SynthesizedExtensionAnalyzer();

  enum class MergeGroupKind : char {
    All,
    MergeableWithTypeDef,
    UnmergeableWithTypeDef,
  };

  void forEachExtensionMergeGroup(MergeGroupKind Kind,
                                  ExtensionGroupOperation Fn);
  bool isInSynthesizedExtension(const ValueDecl *VD);
  bool shouldPrintRequirement(ExtensionDecl *ED, StringRef Req);
  bool hasMergeGroup(MergeGroupKind Kind);
};

class BracketOptions {
  Decl* Target;
  bool OpenExtension;
  bool CloseExtension;
  bool CloseNominal;

public:
  BracketOptions(Decl *Target = nullptr, bool OpenExtension = true,
                 bool CloseExtension = true, bool CloseNominal = true) :
                  Target(Target), OpenExtension(OpenExtension),
                  CloseExtension(CloseExtension),
                  CloseNominal(CloseNominal) {}

  bool shouldOpenExtension(const Decl *D) {
    return D != Target || OpenExtension;
  }

  bool shouldCloseExtension(const Decl *D) {
    return D != Target || CloseExtension;
  }

  bool shouldCloseNominal(const Decl *D) {
    return D != Target || CloseNominal;
  }
};

/// A union of DeclAttrKind and TypeAttrKind.
class AnyAttrKind {
  unsigned kind : 31;
  unsigned isType : 1;

public:
  AnyAttrKind(TypeAttrKind K) : kind(static_cast<unsigned>(K)), isType(1) {
    static_assert(TAK_Count < UINT_MAX, "TypeAttrKind is > 31 bits");
  }
  AnyAttrKind(DeclAttrKind K) : kind(static_cast<unsigned>(K)), isType(0) {
    static_assert(DAK_Count < UINT_MAX, "DeclAttrKind is > 31 bits");
  }
  AnyAttrKind() : kind(TAK_Count), isType(1) {}
  AnyAttrKind(const AnyAttrKind &) = default;

  /// Returns the TypeAttrKind, or TAK_Count if this is not a type attribute.
  TypeAttrKind type() const {
    return isType ? static_cast<TypeAttrKind>(kind) : TAK_Count;
  }
  /// Returns the DeclAttrKind, or DAK_Count if this is not a decl attribute.
  DeclAttrKind decl() const {
    return isType ? DAK_Count : static_cast<DeclAttrKind>(kind);
  }

  bool operator==(AnyAttrKind K) const {
    return kind == K.kind && isType == K.isType;
  }
  bool operator!=(AnyAttrKind K) const { return !(*this == K); }
};

/// Options for printing AST nodes.
///
/// A default-constructed PrintOptions is suitable for printing to users;
/// there are also factory methods for specific use cases.
struct PrintOptions {
  /// \brief The indentation width.
  unsigned Indent = 2;

  /// \brief Whether to print function definitions.
  bool FunctionDefinitions = false;

  /// \brief Whether to print '{ get set }' on readwrite computed properties.
  bool PrintGetSetOnRWProperties = true;

  /// \brief Whether to print *any* accessors on properties.
  bool PrintPropertyAccessors = true;

  /// \brief Whether to print the accessors of a property abstractly,
  /// i.e. always as get and set rather than the specific accessors
  /// actually used to implement the property.
  ///
  /// Printing function definitions takes priority over this setting.
  bool AbstractAccessors = true;

  /// \brief Whether to print type definitions.
  bool TypeDefinitions = false;

  /// \brief Whether to print variable initializers.
  bool VarInitializers = false;

  /// \brief Whether to print a placeholder for default parameters.
  bool PrintDefaultParameterPlaceholder = true;

  /// \brief Whether to print enum raw value expressions.
  bool EnumRawValues = false;

  /// \brief Whether to prefer printing TypeReprs instead of Types,
  /// if a TypeRepr is available.  This allows us to print the original
  /// spelling of the type name.
  ///
  /// \note This should be \c true when printing AST with the intention show
  /// it to the user.
  bool PreferTypeRepr = true;

  /// \brief Whether to print fully qualified Types.
  bool FullyQualifiedTypes = false;

  /// \brief Print fully qualified types if our heuristics say that a certain
  /// type might be ambiguous.
  bool FullyQualifiedTypesIfAmbiguous = false;

  /// \brief Print Swift.Array and Swift.Optional with sugared syntax
  /// ([] and ?), even if there are no sugar type nodes.
  bool SynthesizeSugarOnTypes = false;

  /// \brief If true, the printer will explode a pattern like this:
  /// \code
  ///   var (a, b) = f()
  /// \endcode
  /// into multiple variable declarations.
  ///
  /// For this option to work correctly, \c VarInitializers should be
  /// \c false.
  bool ExplodePatternBindingDecls = false;

  /// If true, the printer will explode an enum case like this:
  /// \code
  ///   case A, B
  /// \endcode
  /// into multiple case declarations.
  bool ExplodeEnumCaseDecls = false;

  /// \brief Whether to print implicit parts of the AST.
  bool SkipImplicit = false;

  /// \brief Whether to print unavailable parts of the AST.
  bool SkipUnavailable = false;

  /// Whether to skip internal stdlib declarations.
  bool SkipPrivateStdlibDecls = false;

  /// Whether to skip underscored stdlib protocols.
  /// Protocols marked with @_show_in_interface are still printed.
  bool SkipUnderscoredStdlibProtocols = false;

  /// Whether to skip extensions that don't add protocols or no members.
  bool SkipEmptyExtensionDecls = true;

  /// Whether to print attributes.
  bool SkipAttributes = false;

  /// Whether to print keywords like 'func'.
  bool SkipIntroducerKeywords = false;

  /// Whether to print destructors.
  bool SkipDeinit = false;

  /// Whether to skip printing 'import' declarations.
  bool SkipImports = false;

  /// \brief Whether to skip printing overrides and witnesses for
  /// protocol requirements.
  bool SkipOverrides = false;

  /// Whether to skip parameter type attributes
  bool SkipParameterTypeAttributes = false;

  /// Whether to print a long attribute like '\@available' on a separate line
  /// from the declaration or other attributes.
  bool PrintLongAttrsOnSeparateLines = false;

  bool PrintImplicitAttrs = true;

  /// Whether to print decl attributes that are only used internally,
  /// such as _silgen_name, transparent, etc.
  bool PrintUserInaccessibleAttrs = true;

  /// List of attribute kinds that should not be printed.
  std::vector<AnyAttrKind> ExcludeAttrList =
      { DAK_Transparent, DAK_Effects, DAK_FixedLayout };

  /// List of attribute kinds that should be printed exclusively.
  /// Empty means allow all.
  std::vector<AnyAttrKind> ExclusiveAttrList;

  /// Whether to print function @convention attribute on function types.
  bool PrintFunctionRepresentationAttrs = true;

  /// Whether to print storage representation attributes on types, e.g.
  /// '@sil_weak', '@sil_unmanaged'.
  bool PrintStorageRepresentationAttrs = false;

  /// Whether to print 'override' keyword on overridden decls.
  bool PrintOverrideKeyword = true;

  /// Whether to print accessibility information on all value decls.
  bool PrintAccessibility = false;

  /// If \c PrintAccessibility is true, this determines whether to print
  /// 'internal' keyword.
  bool PrintInternalAccessibilityKeyword = true;

  /// Print all decls that have at least this level of access.
  Accessibility AccessibilityFilter = Accessibility::Private;

  /// Print IfConfigDecls and IfConfigStmts.
  bool PrintIfConfig = true;

  /// Whether we are printing for sil.
  bool PrintForSIL = false;

  /// Whether we are printing part of SIL body.
  bool PrintInSILBody = false;

  /// Whether to use an empty line to separate two members in a single decl.
  bool EmptyLineBetweenMembers = false;

  /// Whether to print the extensions from conforming protocols.
  bool PrintExtensionFromConformingProtocols = false;

  enum class ArgAndParamPrintingMode {
    ArgumentOnly,
    MatchSource,
    BothAlways,
  };

  /// Whether to print the doc-comment from the conformance if a member decl
  /// has no associated doc-comment by itself.
  bool ElevateDocCommentFromConformance = false;

  /// Whether to print the content of an extension decl inside the type decl where it
  /// extends from.
  std::function<bool(const ExtensionDecl *)> printExtensionContentAsMembers =
    [] (const ExtensionDecl *) { return false; };

  /// How to print the keyword argument and parameter name in functions.
  ArgAndParamPrintingMode ArgAndParamPrinting =
      ArgAndParamPrintingMode::MatchSource;

  /// \brief Whether to print documentation comments attached to declarations.
  /// Note that this may print documentation comments from related declarations
  /// (e.g. the overridden method in the superclass) if such comment is found.
  bool PrintDocumentationComments = false;

  /// \brief Whether to print regular comments from clang module headers.
  bool PrintRegularClangComments = false;

  /// When true, printing interface from a source file will print the original
  /// source text for applicable declarations, in order to preserve the
  /// formatting.
  bool PrintOriginalSourceText = false;

  /// When printing a name alias type, whether print the underlying type instead
  /// of the alias.
  bool PrintNameAliasUnderlyingType = false;

  /// \brief Print dependent types as references into this generic environment.
  GenericEnvironment *GenericEnv = nullptr;

  /// \brief Print types with alternative names from their canonical names.
  llvm::DenseMap<CanType, Identifier> *AlternativeTypeNames = nullptr;

  /// \brief The module in which the printer is used. Determines if the module
  /// name should be printed when printing a type.
  ModuleDecl *CurrentModule = nullptr;

  /// \brief The information for converting archetypes to specialized types.
  llvm::Optional<TypeTransformContext> TransformContext;

  bool PrintAsMember = false;

  /// \brief If this is not \c nullptr then functions (including accessors and
  /// constructors) will be printed with a body that is determined by this
  /// function.
  std::function<std::string(const ValueDecl *)> FunctionBody;

  BracketOptions BracketOptions;

  bool excludeAttrKind(AnyAttrKind K) const {
    if (std::any_of(ExcludeAttrList.begin(), ExcludeAttrList.end(),
                    [K](AnyAttrKind other) { return other == K; }))
      return true;
    if (!ExclusiveAttrList.empty())
      return std::none_of(ExclusiveAttrList.begin(), ExclusiveAttrList.end(),
                          [K](AnyAttrKind other) { return other == K; });
    return false;
  }

  /// Retrieve the set of options for verbose printing to users.
  static PrintOptions printVerbose() {
    PrintOptions result;
    result.TypeDefinitions = true;
    result.VarInitializers = true;
    result.PrintDefaultParameterPlaceholder = true;
    result.PrintDocumentationComments = true;
    result.PrintRegularClangComments = true;
    result.PrintLongAttrsOnSeparateLines = true;
    return result;
  }

  /// Retrieve the set of options suitable for diagnostics printing.
  static PrintOptions printForDiagnostics() {
    PrintOptions result = printVerbose();
    result.PrintAccessibility = true;
    result.Indent = 4;
    result.FullyQualifiedTypesIfAmbiguous = true;
    result.SynthesizeSugarOnTypes = true;
    result.PrintUserInaccessibleAttrs = false;
    result.PrintImplicitAttrs = false;
    result.ExcludeAttrList.push_back(DAK_Exported);
    result.ExcludeAttrList.push_back(DAK_Inline);
    result.ExcludeAttrList.push_back(DAK_Rethrows);
    result.PrintOverrideKeyword = false;
    result.AccessibilityFilter = Accessibility::Public;
    result.PrintIfConfig = false;
    return result;
  }

  /// Retrieve the set of options suitable for interface generation.
  static PrintOptions printInterface() {
    PrintOptions result = printForDiagnostics();
    result.SkipUnavailable = true;
    result.SkipImplicit = true;
    result.SkipPrivateStdlibDecls = true;
    result.SkipUnderscoredStdlibProtocols = true;
    result.SkipDeinit = true;
    result.ExcludeAttrList.push_back(DAK_DiscardableResult);
    result.EmptyLineBetweenMembers = true;
    result.ElevateDocCommentFromConformance = true;
    return result;
  }

  static PrintOptions printTypeInterface(Type T);

  void setBaseType(Type T);

  void initForSynthesizedExtension(NominalTypeDecl *D);

  void clearSynthesizedExtension();

  /// Retrieve the print options that are suitable to print the testable interface.
  static PrintOptions printTestableInterface() {
    PrintOptions result = printInterface();
    result.AccessibilityFilter = Accessibility::Internal;
    return result;
  }

  /// Retrieve the print options that are suitable to print interface for a
  /// swift file.
  static PrintOptions printSwiftFileInterface() {
    PrintOptions result = printInterface();
    result.AccessibilityFilter = Accessibility::Internal;
    result.EmptyLineBetweenMembers = true;
    return result;
  }

  /// Retrieve the set of options suitable for interface generation for
  /// documentation purposes.
  static PrintOptions printDocInterface() {
    PrintOptions result = PrintOptions::printInterface();
    result.PrintAccessibility = false;
    result.SkipUnavailable = false;
    result.ExcludeAttrList.push_back(DAK_Available);
    result.ArgAndParamPrinting =
      PrintOptions::ArgAndParamPrintingMode::BothAlways;
    result.PrintDocumentationComments = false;
    result.PrintRegularClangComments = false;
    result.PrintAccessibility = false;
    result.PrintFunctionRepresentationAttrs = false;
    return result;
  }

  /// Retrieve the set of options suitable for printing SIL functions.
  static PrintOptions printSIL() {
    PrintOptions result;
    result.PrintLongAttrsOnSeparateLines = true;
    result.PrintStorageRepresentationAttrs = true;
    result.AbstractAccessors = false;
    result.PrintForSIL = true;
    result.PrintInSILBody = true;
    result.PreferTypeRepr = false;
    return result;
  }

  static PrintOptions printQualifiedSILType() {
    PrintOptions result = PrintOptions::printSIL();
    result.FullyQualifiedTypesIfAmbiguous = true;
    return result;
  }

  /// \brief Retrieve the set of options that prints everything.
  ///
  /// This is only intended for debug output.
  static PrintOptions printEverything() {
    PrintOptions result = printVerbose();
    result.ExcludeAttrList.clear();
    result.ExcludeAttrList.push_back(DAK_FixedLayout);
    result.PrintStorageRepresentationAttrs = true;
    result.AbstractAccessors = false;
    result.PrintAccessibility = true;
    result.SkipEmptyExtensionDecls = false;
    return result;
  }

  /// Print in the style of quick help declaration.
  static PrintOptions printQuickHelpDeclaration() {
    PrintOptions PO;
    PO.EnumRawValues = true;
    PO.PrintDefaultParameterPlaceholder = true;
    PO.PrintImplicitAttrs = false;
    PO.PrintFunctionRepresentationAttrs = false;
    PO.PrintDocumentationComments = false;
    PO.ExcludeAttrList.push_back(DAK_Available);
    PO.SkipPrivateStdlibDecls = true;
    PO.ExplodeEnumCaseDecls = true;
    return PO;
  }
};
}

#endif // LLVM_SWIFT_AST_PRINTOPTIONS_H
