//===--- PrintOptions.h - AST printing options ------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_PRINTOPTIONS_H
#define SWIFT_AST_PRINTOPTIONS_H

#include "swift/AST/Attr.h"
#include <vector>

namespace swift {
class GenericParamList;
class CanType;
class ExtensionDecl;
class TypeBase;
class DeclContext;
class Type;
enum DeclAttrKind : unsigned;
class PrinterArchetypeTransformer;

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

  /// \brief Whether to print implicit parts of the AST.
  bool SkipImplicit = false;

  /// \brief Whether to print unavailable parts of the AST.
  bool SkipUnavailable = false;

  /// Whether to skip internal stdlib declarations.
  bool SkipPrivateStdlibDecls = false;

  /// Whether to skip underscored stdlib protocols.
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

  /// Whether to print a long attribute like '\@available' on a separate line
  /// from the declaration or other attributes.
  bool PrintLongAttrsOnSeparateLines = false;

  bool PrintImplicitAttrs = true;

  /// Whether to print decl attributes that are only used internally,
  /// such as _silgen_name, transparent, etc.
  bool PrintUserInaccessibleAttrs = true;

  /// List of attribute kinds that should not be printed.
  std::vector<DeclAttrKind> ExcludeAttrList =
      { DAK_Transparent, DAK_Effects, DAK_FixedLayout };

  /// List of attribute kinds that should be printed exclusively.
  /// Empty means allow all.
  std::vector<DeclAttrKind> ExclusiveAttrList;

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

  enum class ArgAndParamPrintingMode {
    ArgumentOnly,
    MatchSource,
    BothAlways,
  };

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

  /// \brief Print dependent types as references into this generic parameter
  /// list.
  GenericParamList *ContextGenericParams = nullptr;
  
  /// \brief Print types with alternative names from their canonical names.
  llvm::DenseMap<CanType, Identifier> *AlternativeTypeNames = nullptr;

  /// \brief When printing a type interface, register the type to print.
  TypeBase *TypeToPrint = nullptr;

  std::shared_ptr<PrinterArchetypeTransformer> pTransformer;

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
    result.SkipDeinit = true;
    return result;
  }

  static PrintOptions printTypeInterface(Type T, DeclContext *DC);

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
    PO.PrintDefaultParameterPlaceholder = true;
    PO.PrintImplicitAttrs = false;
    PO.PrintFunctionRepresentationAttrs = false;
    PO.PrintDocumentationComments = false;
    PO.ExcludeAttrList.push_back(DAK_Available);
    PO.SkipPrivateStdlibDecls = true;
    return PO;
  }
};
}

#endif // LLVM_SWIFT_AST_PRINTOPTIONS_H
