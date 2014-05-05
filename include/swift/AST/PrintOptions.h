//===--- PrintOptions.h - AST printing options ------------------*- C++ -*-===//
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

#ifndef SWIFT_AST_PRINTOPTIONS_H
#define SWIFT_AST_PRINTOPTIONS_H

#include <vector>

namespace swift {
class GenericParamList;
enum DeclAttrKind : unsigned;

struct PrintOptions {
  /// \brief The indentation width.
  unsigned Indent = 2;

  /// \brief Whether to print function definitions.
  bool FunctionDefinitions = false;

  /// \brief Whether to print '{ get set }' on readwrite computed properties.
  bool PrintGetSetOnRWProperties = true;

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

  /// Whether to skip declarations that start with '_'.
  bool SkipLeadingUnderscoreDecls = false;

  bool PrintImplicitAttrs = true;

  /// List of attribute kinds that should not be printed.
  std::vector<DeclAttrKind> ExcludeAttrList;

  /// List of attribute kinds that should be printed exclusively.
  /// Empty means allow all.
  std::vector<DeclAttrKind> ExclusiveAttrList;

  /// Whether to print '@transparent'.
  /// FIXME: Remove this once this attribute becomes a DeclAttr.
  bool PrintAttrTransparent = true;

  /// Whether to print '@unchecked' inside declarations imported from
  /// Objective-C.  If false, print @implicitly unwrapped optionals as normal optionals.
  bool PrintImplicitlyUnwrappedOptionalInImportedDecls = true;

  /// Whether to print '@unchecked' at all.  If false, print @unchecked
  /// optionals as normal optionals.
  bool PrintImplicitlyUnwrappedOptional = true;

  /// Whether to print function representation attributes on function types:
  /// '@thin' or '@objc_block'.
  bool PrintFunctionRepresentationAttrs = true;

  /// Whether to print 'override' keyword on overridden decls.
  bool PrintOverrideKeyword = true;

  /// Whether to always print both the keyword argument and parameter name.
  bool PrintKeywordArgAndParam = false;

  /// \brief Whether to print documentation comments attached to declarations.
  /// Note that this may print documentation comments from related declarations
  /// (e.g. the overridden method in the superclass) if such comment is found.
  bool PrintDocumentationComments = false;

  /// \brief Whether to print regular comments from clang module headers.
  bool PrintRegularClangComments = false;

  /// \brief Print dependent types as references into this generic parameter
  /// list.
  GenericParamList *ContextGenericParams = nullptr;

  /// \brief Retrieve the set of options that prints everything.
  static PrintOptions printEverything() {
    PrintOptions result;
    result.FunctionDefinitions = true;
    result.TypeDefinitions = true;
    result.VarInitializers = true;
    result.PrintDefaultParameterPlaceholder = true;
    result.SkipImplicit = false;
    result.SkipLeadingUnderscoreDecls = false;
    result.PrintImplicitAttrs = true;
    result.PrintAttrTransparent = true;
    result.PrintDocumentationComments = true;
    result.PrintRegularClangComments = true;
    return result;
  }
};
}

#endif // LLVM_SWIFT_AST_PRINTOPTIONS_H
