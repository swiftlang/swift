//===--- ImporterImpl.h - Import Clang Modules - Implementation------------===//
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
// This file provides the implementation class definitions for the Clang
// module loader.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_CLANG_IMPORTER_IMPL_H
#define SWIFT_CLANG_IMPORTER_IMPL_H

#include "swift/ClangImporter/ClangImporter.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Basic/IdentifierTable.h"

namespace clang {
class APValue;
class CompilerInvocation;
class Decl;
class DeclarationName;
class EnumDecl;
class MacroInfo;
class NamedDecl;
class ObjCMethodDecl;
class ParmVarDecl;
class TypedefNameDecl;
class QualType;
}

namespace swift {

class ASTContext;
class ClassDecl;
class ExtensionDecl;
class FuncDecl;
class Identifier;
class Pattern;
class SubscriptDecl;
class Type;
class ValueDecl;

/// \brief Describes the kind of conversion to apply to a constant value.
enum class ConstantConvertKind {
  /// \brief No conversion required.
  None,
  /// \brief Coerce the constant to the given type.
  Coerce,
  /// \brief Construct the given type from the constant value.
  Construction,
  /// \brief Perform an unchecked downcast to the given type.
  Downcast
};

/// \brief Describes the kind of type import we're performing.
enum class ImportTypeKind {
  /// \brief A "normal' type import, with no special rules.
  Normal,
  /// \brief Import the result type of a function.
  ///
  /// This provides special treatment for 'void', among other things.
  Result,
  /// \brief Import the type of a function parameter.
  ///
  /// This provides special treatment for C++ references (which become
  /// [byref] parameters), among other things.
  Parameter
};

/// \brief Implementation of the Clang importer.
struct ClangImporter::Implementation {
  /// \brief Describes how a particular C enumeration type will be imported
  /// into Swift. All of the possibilities have the same storage
  /// representation, but can be used in different ways.
  enum class EnumKind {
    /// \brief The enumeration type should map to a oneof, which means that
    /// all of the options are independent.
    OneOf,
    /// \brief The enumeration type should map to a distinct type that acts
    /// as a bitset, for which each of the possible values represents a
    /// specific bit.
    Options,
    /// \brief The enumeration type should simply map to the appropriate
    /// integer
    Constants
  };

  Implementation(ASTContext &ctx) : SwiftContext(ctx) { }

  /// \brief Swift AST context.
  ASTContext &SwiftContext;

  /// \brief A count of the number of load module operations.
  /// FIXME: Horrible, horrible hack for \c loadModule().
  unsigned ImportCounter = 0;

  /// \brief Clang compiler invocation.
  llvm::IntrusiveRefCntPtr<clang::CompilerInvocation> Invocation;

  /// \brief Clang compiler instance, which is used to actually load Clang
  /// modules.
  std::unique_ptr<clang::CompilerInstance> Instance;

  /// \brief Clang compiler action, which is used to actually run the
  /// parser.
  std::unique_ptr<clang::SyntaxOnlyAction> Action;

  /// \brief Mapping of already-imported declarations.
  llvm::DenseMap<clang::Decl *, Decl *> ImportedDecls;

  /// \brief The set of "special" typedef-name declarations, which are
  /// mapped to specific Swift types.
  ///
  /// Normal typedef-name declarations imported into Swift will maintain
  /// equality between the imported declaration's underlying type and the
  /// import of the underlying type. A typedef-name declaration is special
  /// when this is not the case, e.g., Objective-C's "BOOL" has an underlying
  /// type of "signed char", but is mapped to a special Swift struct type
  /// ObjCBool.
  llvm::SmallPtrSet<clang::TypedefNameDecl *, 8> SpecialTypedefNames;

  /// \brief Mapping of already-imported declarations from protocols, which
  /// can (and do) get replicated into classes.
  llvm::DenseMap<std::pair<clang::Decl *, DeclContext *>, Decl *>
    ImportedProtocolDecls;

  /// \brief Mapping of already-imported macros.
  llvm::DenseMap<clang::MacroInfo *, ValueDecl *> ImportedMacros;

  /// \brief Generation number that is used for crude versioning.
  ///
  /// This value is incremented every time a new module is imported.
  unsigned Generation = 1;

  /// \brief A cached set of extensions for a particular Objective-C class.
  struct CachedExtensions {
    CachedExtensions()
      : Extensions(nullptr), Generation(0) { }

    CachedExtensions(const CachedExtensions &) = delete;
    CachedExtensions &operator=(const CachedExtensions &) = delete;

    CachedExtensions(CachedExtensions &&other)
      : Extensions(other.Extensions), Generation(other.Generation)
    {
      other.Extensions = nullptr;
      other.Generation = 0;
    }

    CachedExtensions &operator=(CachedExtensions &&other) {
      delete Extensions;
      Extensions = other.Extensions;
      Generation = other.Generation;
      other.Extensions = nullptr;
      other.Generation = 0;
      return *this;
    }

    ~CachedExtensions() { delete Extensions; }

    /// \brief The cached extensions.
    SmallVector<ExtensionDecl *, 4> *Extensions;

    /// \brief Generation number used to tell when this cache has gone stale.
    unsigned Generation;
  };

  /// \brief Cache of the class extensions.
  llvm::DenseMap<ClassDecl *, CachedExtensions> ClassExtensions;

  /// \brief Keep track of subscript declarations based on getter/setter
  /// pairs.
  llvm::DenseMap<std::pair<FuncDecl *, FuncDecl *>, SubscriptDecl *> Subscripts;

private:
  /// \brief NSObject, imported into Swift.
  Type NSObjectTy;

  ///\ brief The Swift standard library module.
  Module *swiftModule = nullptr;

public:
  /// \brief The first Clang module we loaded.
  ///
  /// FIXME: This horrible hack is used because we don't have a nice way to
  /// map from a Decl in the tree back to the appropriate Clang module.
  /// It also means building ClangModules for all of the dependencies of a
  /// Clang module.
  ClangModule *firstClangModule = nullptr;

  /// \brief Clang's objectAtIndexedSubscript: selector.
  clang::Selector objectAtIndexedSubscript;

  /// \brief Clang's setObjectAt:indexedSubscript: selector.
  clang::Selector setObjectAtIndexedSubscript;

  /// \brief Clang's objectForKeyedSubscript: selector.
  clang::Selector objectForKeyedSubscript;

  /// \brief Clang's setObject:forKeyedSubscript: selector.
  clang::Selector setObjectForKeyedSubscript;
  
  /// \brief Retrieve the Clang AST context.
  clang::ASTContext &getClangASTContext() const {
    return Instance->getASTContext();
  }

  /// \brief Import the given Swift identifier into Clang.
  clang::DeclarationName importName(Identifier name);
  
  /// \brief Import the given Clang name into Swift.
  ///
  /// \param name The Clang name to map into Swift.
  ///
  /// \param suffix The suffix to append to the Clang name to produce the
  /// Swift name.
  Identifier importName(clang::DeclarationName name, StringRef suffix = "");

  /// \brief Import the given Swift source location into Clang.
  clang::SourceLocation importSourceLoc(SourceLoc loc);

  /// \brief Import the given Clang source location into Swift.
  SourceLoc importSourceLoc(clang::SourceLocation loc);

  /// \brief Import the given Clang source range into Swift.
  SourceRange importSourceRange(clang::SourceRange loc);

  /// \brief Import the given Clang preprocessor macro as a Swift value decl.
  ///
  /// \returns The imported declaration, or null if the macro could not be
  /// translated into Swift.
  ValueDecl *importMacro(Identifier name, clang::MacroInfo *macro);

  /// \brief Classify the given Clang enumeration type to describe how it
  /// should be imported 
  EnumKind classifyEnum(clang::EnumDecl *decl);

  /// \brief Import the given Clang declaration into Swift.
  ///
  /// \returns The imported declaration, or null if this declaration could
  /// not be represented in Swift.
  Decl *importDecl(clang::NamedDecl *decl);

  /// \brief Import a cloned version of the given declaration, which is part of
  /// an Objective-C protocol and currently must be a method, into the given
  /// declaration context.
  ///
  /// \returns The imported declaration, or null if this declaration could not
  /// be represented in Swift.
  Decl *importMirroredDecl(clang::ObjCMethodDecl *decl, DeclContext *dc);

  /// \brief Import the given Clang declaration context into Swift.
  ///
  /// \returns The imported declaration context, or null if it could not
  /// be converted.
  DeclContext *importDeclContext(clang::DeclContext *dc);

  /// \brief Create a new named constant with the given value.
  ///
  /// \param name The name of the constant.
  /// \param dc The declaration context into which the name will be introduced.
  /// \param type The type of the named constant.
  /// \param value The value of the named constant.
  /// \param convertKind How to convert the constant to the given type.
  ValueDecl *createConstant(Identifier name, DeclContext *dc,
                            Type type, const clang::APValue &value,
                            ConstantConvertKind convertKind);

  /// \brief Retrieve the 'swift' module.
  ///
  /// \returns The 'swift' module, or null if the module has not been imported.
  Module *getSwiftModule();

  /// \brief Retrieve the named module.
  ///
  /// \param The name of the module.
  ///
  /// \returns The named module, or null if the module has not been imported.
  Module *getNamedModule(StringRef name);

  /// \brief Retrieve the named Swift type, e.g., Int32.
  ///
  /// \param module The name of the module in which the type should occur.
  ///
  /// \param name The name of the type to find.
  ///
  /// \returns The named type, or null if the type could not be found.
  Type getNamedSwiftType(Module *module, StringRef name);

  /// \brief Retrieve a specialization of the the named Swift type, e.g.,
  /// CPointer<T>.
  ///
  /// \param module The name of the module in which the type should occur.
  ///
  /// \param name The name of the type to find.
  ///
  /// \param args The arguments to use in the specialization.
  ///
  /// \returns The named type, or null if the type could not be found.
  Type getNamedSwiftTypeSpecialization(Module *module, StringRef name,
                                       ArrayRef<Type> args);

  /// \brief Retrieve the NSObject type.
  Type getNSObjectType();

  /// \brief Import the given Clang type into Swift.
  ///
  /// \param type The Clang type to import.
  ///
  /// \param kind The kind of type import we're performing.
  ///
  /// \returns The imported type, or null if this type could
  /// not be represented in Swift.
  Type importType(clang::QualType type, ImportTypeKind kind);

  /// \brief Import the given function type.
  ///
  /// This routine should be preferred when importing function types for
  /// which we have actual function parameters, e.g., when dealing with a
  /// function declaration, because it produces a function type whose input
  /// tuple has argument names.
  ///
  /// \param resultType The result type of the function.
  /// \param params The parameter types to the function.
  /// \param isVariadic Whether the function is variadic.
  /// \param argPatterns The externally-visible patterns for the parameters.
  /// \param bodyPatterns The patterns visible inside the function body.
  /// \param selector The Objective-C method selector to use for the names.
  /// \param isConstructor Whether we're building a function type for a
  /// constructor.
  ///
  /// \returns the imported function type, or null if the type cannot be
  /// imported.
  Type importFunctionType(clang::QualType resultType,
                          ArrayRef<clang::ParmVarDecl *> params,
                          bool isVariadic,
                          SmallVectorImpl<Pattern*> &argPatterns,
                          SmallVectorImpl<Pattern*> &bodyPatterns,
                          clang::Selector selector = clang::Selector(),
                          bool isConstructor = false);

  /// \brief Determine whether the given typedef-name is "special", meaning
  /// that it has performed some non-trivial mapping of its underlying type
  /// based on the name of the typedef.
  bool isSpecialTypedefName(clang::TypedefNameDecl *decl) {
    return SpecialTypedefNames.count(decl) > 0;
  }
};

}

#endif
