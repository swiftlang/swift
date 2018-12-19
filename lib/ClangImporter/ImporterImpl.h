//===--- ImporterImpl.h - Import Clang Modules: Implementation --*- C++ -*-===//
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
//
// This file provides the implementation class definitions for the Clang
// module loader.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_CLANG_IMPORTER_IMPL_H
#define SWIFT_CLANG_IMPORTER_IMPL_H

#include "ClangAdapter.h"
#include "ImportEnumInfo.h"
#include "ImportName.h"
#include "SwiftLookupTable.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/Type.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/Basic/FileTypes.h"
#include "swift/Basic/StringExtras.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Serialization/ModuleFileExtension.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/Path.h"
#include <set>

namespace llvm {

class SmallBitVector;

}

namespace clang {
class APValue;
class Decl;
class DeclarationName;
class EnumDecl;
class MacroInfo;
class MangleContext;
class NamedDecl;
class ObjCInterfaceDecl;
class ObjCMethodDecl;
class ObjCPropertyDecl;
class ParmVarDecl;
class Parser;
class QualType;
class TypedefNameDecl;
}

namespace swift {

class ASTContext;
class ClassDecl;
class ConstructorDecl;
class Decl;
class DeclContext;
class Expr;
class ExtensionDecl;
class FuncDecl;
class Identifier;
class Pattern;
class SubscriptDecl;
class ValueDecl;

/// \brief Describes the kind of conversion to apply to a constant value.
enum class ConstantConvertKind {
  /// \brief No conversion required.
  None,
  /// \brief Coerce the constant to the given type.
  Coerce,
  /// \brief Construct the given type from the constant value.
  Construction,
  /// \brief Construct the given type from the constant value, using an
  /// optional initializer.
  ConstructionWithUnwrap,
  /// \brief Perform an unchecked downcast to the given type.
  Downcast
};

/// \brief Describes the kind of type import we're performing.
enum class ImportTypeKind {
  /// \brief Import a type in its most abstract form, without any adjustment.
  Abstract,

  /// \brief Import the underlying type of a typedef.
  Typedef,

  /// \brief Import the type of a literal value.
  Value,

  /// \brief Import the type of an Objective-C generic argument.
  ObjCCollectionElement,

  /// \brief Import the declared type of a variable.
  Variable,
  
  /// \brief Import the declared type of an audited variable.
  ///
  /// This is exactly like ImportTypeKind::Variable, except it
  /// disables wrapping CF class types in Unmanaged.
  AuditedVariable,

  /// \brief Import the declared type of a struct or union field.
  RecordField,
  
  /// \brief Import the result type of a function.
  ///
  /// This provides special treatment for 'void', among other things, and
  /// enables the conversion of bridged types.
  Result,

  /// \brief Import the result type of an audited function.
  ///
  /// This is exactly like ImportTypeKind::Result, except it
  /// disables wrapping CF class types in Unmanaged.
  AuditedResult,

  /// \brief Import the type of a function parameter.
  ///
  /// This provides special treatment for C++ references (which become
  /// [inout] parameters) and C pointers (which become magic [inout]-able types),
  /// among other things, and enables the conversion of bridged types.
  /// Parameters are always considered CF-audited.
  Parameter,

  /// \brief Import the type of a parameter declared with
  /// \c CF_RETURNS_RETAINED.
  ///
  /// This ensures that the parameter is not marked as Unmanaged.
  CFRetainedOutParameter,

  /// \brief Import the type of a parameter declared with
  /// \c CF_RETURNS_NON_RETAINED.
  ///
  /// This ensures that the parameter is not marked as Unmanaged.
  CFUnretainedOutParameter,

  /// \brief Import the type pointed to by a pointer or reference.
  ///
  /// This provides special treatment for pointer-to-ObjC-pointer
  /// types, which get imported as pointers to *checked* optional,
  /// *Pointer<NSFoo?>, instead of implicitly unwrapped optional as usual.
  Pointee,

  /// \brief Import the type of an ObjC property.
  ///
  /// This enables the conversion of bridged types. Properties are always
  /// considered CF-audited.
  Property,

  /// \brief Import the type of an ObjC property accessor marked 'weak',
  /// 'assign', or 'unsafe_unretained'.
  ///
  /// Like Property, but doesn't allow bridging to a value type, since that
  /// would discard the ownership.
  PropertyWithReferenceSemantics,

  /// \brief Import the underlying type of an enum.
  ///
  /// This provides special treatment for 'NSUInteger'.
  Enum
};

/// Controls whether a typedef for \p type should name the fully-bridged Swift
/// type or the original Clang type.
///
/// In either case we end up losing sugar at some uses sites, so this is more
/// about what the right default is.
static inline Bridgeability getTypedefBridgeability(
                                          const clang::TypedefNameDecl *decl,
                                          clang::QualType type) {
    return decl->hasAttr<clang::SwiftBridgedTypedefAttr>()
      ? Bridgeability::Full
        : type->isBlockPointerType()
        ? Bridgeability::Full
          : Bridgeability::None;
}

/// \brief Describes the kind of the C type that can be mapped to a stdlib
/// swift type.
enum class MappedCTypeKind {
  UnsignedInt,
  SignedInt,
  UnsignedWord,
  SignedWord,
  FloatIEEEsingle,
  FloatIEEEdouble,
  FloatX87DoubleExtended,
  VaList,
  ObjCBool,
  ObjCSel,
  ObjCId,
  ObjCClass,
  CGFloat,
  Block,
};

/// \brief Describes what to do with the C name of a type that can be mapped to
/// a Swift standard library type.
enum class MappedTypeNameKind {
  DoNothing,
  DefineOnly,
  DefineAndUse
};

/// \brief Describes certain kinds of methods that need to be specially
/// handled by the importer.
enum class SpecialMethodKind {
  Regular,
  Constructor,
  NSDictionarySubscriptGetter
};

#define SWIFT_NATIVE_ANNOTATION_STRING "__swift native"

#define SWIFT_PROTOCOL_SUFFIX "Protocol"
#define SWIFT_CFTYPE_SUFFIX "Ref"

/// Describes whether to classify a factory method as an initializer.
enum class FactoryAsInitKind {
  /// Infer based on name and type (the default).
  Infer,
  /// Treat as a class method.
  AsClassMethod,
  /// Treat as an initializer.
  AsInitializer
};

namespace importer {
struct PlatformAvailability {
  /// A predicate that indicates if the given platform should be
  /// considered for availability.
  std::function<bool(StringRef PlatformName)> filter;

  /// A predicate that indicates if the given platform version should
  /// should be included in the cutoff of deprecated APIs marked unavailable.
  std::function<bool(unsigned major, llvm::Optional<unsigned> minor)>
      deprecatedAsUnavailableFilter;

  /// The message to embed for implicitly unavailability if a deprecated
  /// API is now unavailable.
  std::string deprecatedAsUnavailableMessage;

  PlatformAvailability(LangOptions &opts);

private:
  PlatformAvailability(const PlatformAvailability&) = delete;
  PlatformAvailability &operator=(const PlatformAvailability &) = delete;
};
}

using LookupTableMap =
    llvm::DenseMap<StringRef, std::unique_ptr<SwiftLookupTable>>;

/// The result of importing a clang type. It holds both the Swift Type
/// as well as a bool in which 'true' indicates either:
///   This is an Optional type.
///   This is a function type where the result type is an Optional.
/// It is otherwise 'false'.
class ImportedType {
  Type type;
  bool isIUO;

public:
  ImportedType() {
    type = Type();
    isIUO = false;
  }

  ImportedType(Type ty, bool implicitlyUnwrap)
      : type(ty), isIUO(implicitlyUnwrap) {
#if !defined(NDEBUG)
    if (implicitlyUnwrap) {
      assert(ty->getOptionalObjectType() || ty->getAs<AnyFunctionType>());
      if (!ty->getOptionalObjectType()) {
        auto fnTy = ty->castTo<AnyFunctionType>();
        assert(fnTy->getResult()->getOptionalObjectType());
      }
    }
#endif
  }

  Type getType() const { return type; }

  bool isImplicitlyUnwrapped() const { return isIUO; }

  // Allow a direct test in boolean contexts. It makes sense to base
  // this entirely on the type as the isIUO is meaningless for a null
  // type.
  explicit operator bool() const { return type.getPointer() != nullptr; }
};

/// \brief Implementation of the Clang importer.
class LLVM_LIBRARY_VISIBILITY ClangImporter::Implementation 
  : public LazyMemberLoader,
    public LazyConformanceLoader
{
  friend class ClangImporter;
  using Version = importer::ImportNameVersion;

public:
  Implementation(ASTContext &ctx, const ClangImporterOptions &opts);
  ~Implementation();

  /// \brief Swift AST context.
  ASTContext &SwiftContext;

  const bool ImportForwardDeclarations;
  const bool InferImportAsMember;
  const bool DisableSwiftBridgeAttr;
  const bool BridgingHeaderExplicitlyRequested;
  const bool DisableAdapterModules;

  bool IsReadingBridgingPCH;
  llvm::SmallVector<clang::serialization::SubmoduleID, 2> PCHImportedSubmodules;

  const Version CurrentVersion;

  constexpr static const char * const moduleImportBufferName =
    "<swift-imported-modules>";
  constexpr static const char * const bridgingHeaderBufferName =
    "<bridging-header-import>";

private:
  /// The Swift lookup table for the bridging header.
  std::unique_ptr<SwiftLookupTable> BridgingHeaderLookupTable;

  /// The Swift lookup tables, per module.
  ///
  /// Annoyingly, we list this table early so that it gets torn down after
  /// the underlying Clang instances that reference it
  /// (through the Swift name lookup module file extension).
  LookupTableMap LookupTables;

  /// \brief The fake buffer used to import modules.
  ///
  /// \see getNextIncludeLoc
  clang::FileID DummyIncludeBuffer;

  /// \brief A count of the number of load module operations.
  ///
  /// \see getNextIncludeLoc
  unsigned IncludeCounter = 0;

  /// Generate a dummy Clang source location for header includes and module
  /// imports.
  ///
  /// These have to be unique and valid or Clang gets very confused.
  clang::SourceLocation getNextIncludeLoc();

  /// \brief Used to avoid running the AST verifier over the same declarations.
  size_t VerifiedDeclsCounter = 0;

  /// \brief Clang compiler invocation.
  std::shared_ptr<clang::CompilerInvocation> Invocation;

  /// \brief Clang compiler instance, which is used to actually load Clang
  /// modules.
  std::unique_ptr<clang::CompilerInstance> Instance;

  /// \brief Clang compiler action, which is used to actually run the
  /// parser.
  std::unique_ptr<clang::FrontendAction> Action;

  /// \brief Clang parser, which is used to load textual headers.
  std::unique_ptr<clang::Parser> Parser;

  /// \brief Clang parser, which is used to load textual headers.
  std::unique_ptr<clang::MangleContext> Mangler;

  /// The active type checker, or null if there is no active type checker.
  ///
  /// The flag is \c true if there has ever been a type resolver assigned, i.e.
  /// if type checking has begun.
  llvm::PointerIntPair<LazyResolver *, 1, bool> typeResolver;

public:
  /// \brief Mapping of already-imported declarations.
  llvm::DenseMap<std::pair<const clang::Decl *, Version>, Decl *> ImportedDecls;

  /// \brief The set of "special" typedef-name declarations, which are
  /// mapped to specific Swift types.
  ///
  /// Normal typedef-name declarations imported into Swift will maintain
  /// equality between the imported declaration's underlying type and the
  /// import of the underlying type. A typedef-name declaration is special
  /// when this is not the case, e.g., Objective-C's "BOOL" has an underlying
  /// type of "signed char", but is mapped to a special Swift struct type
  /// ObjCBool.
  llvm::SmallDenseMap<const clang::TypedefNameDecl *, MappedTypeNameKind, 16>
    SpecialTypedefNames;

  /// \brief Provide a single extension point for any given type per clang
  /// submodule
  llvm::DenseMap<std::pair<NominalTypeDecl *, const clang::Module *>,
                 ExtensionDecl *> extensionPoints;

  /// \brief Typedefs that we should not be importing.  We should be importing
  /// underlying decls instead.
  llvm::DenseSet<const clang::Decl *> SuperfluousTypedefs;

  /// Tag decls whose typedefs were imported instead.
  ///
  /// \sa SuperfluousTypedefs
  llvm::DenseSet<const clang::Decl *> DeclsWithSuperfluousTypedefs;

  /// \brief Mapping of already-imported declarations from protocols, which
  /// can (and do) get replicated into classes.
  llvm::DenseMap<std::tuple<const clang::Decl *, DeclContext *, Version>,
                 Decl *> ImportedProtocolDecls;

  /// Mapping from identifiers to the set of macros that have that name along
  /// with their corresponding Swift declaration.
  ///
  /// Multiple macro definitions can map to the same declaration if the
  /// macros are identically defined.
  llvm::DenseMap<Identifier,
                 SmallVector<std::pair<const clang::MacroInfo *, ValueDecl *>,
                             2>>
    ImportedMacros;

  // Mapping from macro to value for macros that expand to constant values.
  llvm::DenseMap<const clang::MacroInfo *, std::pair<clang::APValue, Type>>
    ImportedMacroConstants;

  /// Keeps track of active selector-based lookups, so that we don't infinitely
  /// recurse when checking whether a method with a given selector has already
  /// been imported.
  llvm::DenseMap<std::pair<ObjCSelector, char>, unsigned>
    ActiveSelectors;

  clang::CompilerInstance *getClangInstance() {
    return Instance.get();
  }

private:
  /// \brief Generation number that is used for crude versioning.
  ///
  /// This value is incremented every time a new module is imported.
  unsigned Generation = 1;

  void bumpGeneration() {
    ++Generation;
    SwiftContext.bumpGeneration();
  }

public:
  /// \brief Keep track of subscript declarations based on getter/setter
  /// pairs.
  llvm::DenseMap<std::pair<FuncDecl *, FuncDecl *>, SubscriptDecl *> Subscripts;

  /// Keeps track of the Clang functions that have been turned into
  /// properties.
  llvm::DenseMap<const clang::FunctionDecl *, VarDecl *> FunctionsAsProperties;

  importer::EnumInfo getEnumInfo(const clang::EnumDecl *decl) {
    return getNameImporter().getEnumInfo(decl);
  }
  importer::EnumKind getEnumKind(const clang::EnumDecl *decl) {
    return getNameImporter().getEnumKind(decl);
  }

private:
  /// A mapping from imported declarations to their "alternate" declarations,
  /// for cases where a single Clang declaration is imported to two
  /// different Swift declarations.
  llvm::DenseMap<Decl *, TinyPtrVector<ValueDecl *>> AlternateDecls;

public:
  /// \brief Keep track of initializer declarations that correspond to
  /// imported methods.
  llvm::DenseMap<
      std::tuple<const clang::ObjCMethodDecl *, DeclContext *, Version>,
      ConstructorDecl *> Constructors;

  /// Retrieve the alternative declaration for the given imported
  /// Swift declaration.
  ArrayRef<ValueDecl *> getAlternateDecls(Decl *decl) {
    auto known = AlternateDecls.find(decl);
    if (known == AlternateDecls.end()) return {};
    return known->second;
  }

  /// Add an alternative decl
  void addAlternateDecl(Decl *forDecl, ValueDecl *altDecl) {
    auto &vec = AlternateDecls[forDecl];
    for (auto alt : vec)
      if (alt == altDecl)
        return;
    vec.push_back(altDecl);
  }

private:
  /// \brief NSObject, imported into Swift.
  Type NSObjectTy;

  /// A pair containing a ClangModuleUnit,
  /// and whether the adapters of its re-exported modules have all been forced
  /// to load already.
  using ModuleInitPair = llvm::PointerIntPair<ClangModuleUnit *, 1, bool>;

public:
  /// A map from Clang modules to their Swift wrapper modules.
  llvm::SmallDenseMap<const clang::Module *, ModuleInitPair, 16> ModuleWrappers;

  /// The module unit that contains declarations from imported headers.
  ClangModuleUnit *ImportedHeaderUnit = nullptr;

  /// The modules re-exported by imported headers.
  llvm::SmallVector<clang::Module *, 8> ImportedHeaderExports;

  /// The modules that requested imported headers.
  ///
  /// These are used to look up Swift classes forward-declared with \@class.
  TinyPtrVector<ModuleDecl *> ImportedHeaderOwners;

  /// \brief Clang's objectAtIndexedSubscript: selector.
  clang::Selector objectAtIndexedSubscript;

  /// \brief Clang's setObjectAt:indexedSubscript: selector.
  clang::Selector setObjectAtIndexedSubscript;

  /// \brief Clang's objectForKeyedSubscript: selector.
  clang::Selector objectForKeyedSubscript;

  /// \brief Clang's setObject:forKeyedSubscript: selector.
  clang::Selector setObjectForKeyedSubscript;

private:
  /// Records those modules that we have looked up.
  llvm::DenseMap<Identifier, ModuleDecl *> checkedModules;

  /// Mapping from delayed conformance IDs to the set of delayed
  /// protocol conformances.
  llvm::DenseMap<unsigned, SmallVector<ProtocolConformance *, 4>>
    DelayedConformances;

  /// The next delayed conformance ID to use with \c DelayedConformances.
  unsigned NextDelayedConformanceID = 0;

  /// The set of imported protocols for a declaration, used only to
  /// load all members of the declaration.
  llvm::DenseMap<const Decl *, SmallVector<ProtocolDecl *, 4>>
    ImportedProtocols;

  void startedImportingEntity();

public:
  importer::PlatformAvailability platformAvailability;

private:
  /// For importing names. This is initialized by the ClangImporter::create()
  /// after having set up a suitable Clang instance.
  std::unique_ptr<importer::NameImporter> nameImporter = nullptr;

  /// If there is a single .PCH file imported into the __ObjC module, this
  /// is the filename of that PCH. When other files are imported, this should
  /// be llvm::None.
  Optional<std::string> SinglePCHImport = None;

public:
  importer::NameImporter &getNameImporter() {
    assert(nameImporter && "haven't finished initialization");
    return *nameImporter;
  }

  /// Tracks top level decls from the bridging header.
  std::vector<clang::Decl *> BridgeHeaderTopLevelDecls;
  std::vector<llvm::PointerUnion<clang::ImportDecl *, ImportDecl *>>
    BridgeHeaderTopLevelImports;

  /// Tracks macro definitions from the bridging header.
  std::vector<clang::IdentifierInfo *> BridgeHeaderMacros;
  /// Tracks included headers from the bridging header.
  llvm::DenseSet<const clang::FileEntry *> BridgeHeaderFiles;

  void addBridgeHeaderTopLevelDecls(clang::Decl *D);
  bool shouldIgnoreBridgeHeaderTopLevelDecl(clang::Decl *D);

public:
  void registerExternalDecl(Decl *D) {
    if (!hasFinishedTypeChecking())
      SwiftContext.addExternalDecl(D);
  }

  void recordImplicitUnwrapForDecl(Decl *decl, bool isIUO) {
#if !defined(NDEBUG)
    Type ty;
    if (auto *FD = dyn_cast<FuncDecl>(decl)) {
      assert(FD->getInterfaceType());
      ty = FD->getResultInterfaceType();
    } else if (auto *CD = dyn_cast<ConstructorDecl>(decl)) {
      assert(CD->getInterfaceType());
      ty = CD->getResultInterfaceType();
    } else {
      ty = cast<AbstractStorageDecl>(decl)->getValueInterfaceType();
    }
#endif

    if (!isIUO)
      return;

    assert(ty->getOptionalObjectType());

    auto *IUOAttr = new (SwiftContext)
        ImplicitlyUnwrappedOptionalAttr(/* implicit= */ true);
    decl->getAttrs().add(IUOAttr);
  }

  /// \brief Retrieve the Clang AST context.
  clang::ASTContext &getClangASTContext() const {
    return Instance->getASTContext();
  }

  /// \brief Retrieve the Clang Sema object.
  clang::Sema &getClangSema() const {
    return Instance->getSema();
  }

  /// \brief Retrieve the Clang AST context.
  clang::Preprocessor &getClangPreprocessor() const {
    return Instance->getPreprocessor();
  }
  
  clang::CodeGenOptions &getClangCodeGenOpts() const {
    return Instance->getCodeGenOpts();
  }

  /// Imports the given header contents into the Clang context.
  bool importHeader(ModuleDecl *adapter, StringRef headerName,
                    SourceLoc diagLoc, bool trackParsedSymbols,
                    std::unique_ptr<llvm::MemoryBuffer> contents,
                    bool implicitImport);

  /// \brief Retrieve the imported module that should contain the given
  /// Clang decl.
  ClangModuleUnit *getClangModuleForDecl(const clang::Decl *D,
                                         bool allowForwardDeclaration = false);

  /// Returns the module \p MI comes from, or \c None if \p MI does not have
  /// a valid associated module.
  ///
  /// The returned module may be null (but not \c None) if \p MI comes from
  /// an imported header.
  const clang::Module *getClangOwningModule(ClangNode Node) const;

  /// Whether NSUInteger can be imported as Int in certain contexts. If false,
  /// should always be imported as UInt.
  static bool shouldAllowNSUIntegerAsInt(bool isFromSystemModule,
                                         const clang::NamedDecl *decl);

  /// \brief Converts the given Swift identifier for Clang.
  clang::DeclarationName exportName(Identifier name);

  /// Imports the full name of the given Clang declaration into Swift.
  ///
  /// Note that this may result in a name very different from the Clang name,
  /// so it should not be used when referencing Clang symbols.
  ///
  /// \param D The Clang declaration whose name should be imported.
  importer::ImportedName importFullName(const clang::NamedDecl *D,
                                        Version version,
                                        clang::DeclarationName givenName =
                                          clang::DeclarationName()) {
    return getNameImporter().importName(D, version, givenName);
  }

  /// Print an imported name as a string suitable for the swift_name attribute,
  /// or the 'Rename' field of AvailableAttr.
  void printSwiftName(importer::ImportedName name,
                      importer::ImportNameVersion version,
                      bool fullyQualified,
                      llvm::raw_ostream &os);

  /// \brief Import the given Clang identifier into Swift.
  ///
  /// \param identifier The Clang identifier to map into Swift.
  ///
  /// \param removePrefix The prefix to remove from the Clang name to produce
  /// the Swift name. If the Clang name does not start with this prefix,
  /// nothing is removed.
  Identifier importIdentifier(const clang::IdentifierInfo *identifier,
                              StringRef removePrefix = "");

  /// Import an Objective-C selector.
  ObjCSelector importSelector(clang::Selector selector);

  /// Import a Swift name as a Clang selector.
  clang::Selector exportSelector(DeclName name, bool allowSimpleName = true);

  /// Export a Swift Objective-C selector as a Clang Objective-C selector.
  clang::Selector exportSelector(ObjCSelector selector);

  /// \brief Import the given Swift source location into Clang.
  clang::SourceLocation exportSourceLoc(SourceLoc loc);

  /// \brief Import the given Clang source location into Swift.
  SourceLoc importSourceLoc(clang::SourceLocation loc);

  /// \brief Import the given Clang source range into Swift.
  SourceRange importSourceRange(clang::SourceRange loc);

  /// \brief Import the given Clang preprocessor macro as a Swift value decl.
  ///
  /// \p macroNode must be a MacroInfo or a ModuleMacro.
  ///
  /// \returns The imported declaration, or null if the macro could not be
  /// translated into Swift.
  ValueDecl *importMacro(Identifier name, ClangNode macroNode);

  /// Map a Clang identifier name to its imported Swift equivalent.
  StringRef getSwiftNameFromClangName(StringRef name);

  /// Import attributes from the given Clang declaration to its Swift
  /// equivalent.
  ///
  /// \param ClangDecl The decl being imported.
  /// \param MappedDecl The decl to attach attributes to.
  /// \param NewContext If present, the Clang node for the context the decl is
  /// being imported into, which may affect info from API notes.
  void importAttributes(const clang::NamedDecl *ClangDecl, Decl *MappedDecl,
                        const clang::ObjCContainerDecl *NewContext = nullptr);

  /// If we already imported a given decl, return the corresponding Swift decl.
  /// Otherwise, return nullptr.
  Decl *importDeclCached(const clang::NamedDecl *ClangDecl, Version version);

  Decl *importDeclImpl(const clang::NamedDecl *ClangDecl, Version version,
                       bool &TypedefIsSuperfluous, bool &HadForwardDeclaration);

  Decl *importDeclAndCacheImpl(const clang::NamedDecl *ClangDecl,
                               Version version,
                               bool SuperfluousTypedefsAreTransparent);

  /// \brief Same as \c importDeclReal, but for use inside importer
  /// implementation.
  ///
  /// Unlike \c importDeclReal, this function for convenience transparently
  /// looks through superfluous typedefs and returns the imported underlying
  /// decl in that case.
  Decl *importDecl(const clang::NamedDecl *ClangDecl, Version version) {
    return importDeclAndCacheImpl(ClangDecl, version,
                                  /*SuperfluousTypedefsAreTransparent=*/true);
  }

  /// \brief Import the given Clang declaration into Swift.  Use this function
  /// outside of the importer implementation, when importing a decl requested by
  /// Swift code.
  ///
  /// \returns The imported declaration, or null if this declaration could
  /// not be represented in Swift.
  Decl *importDeclReal(const clang::NamedDecl *ClangDecl, Version version) {
    return importDeclAndCacheImpl(ClangDecl, version,
                                  /*SuperfluousTypedefsAreTransparent=*/false);
  }

  /// \brief Import a cloned version of the given declaration, which is part of
  /// an Objective-C protocol and currently must be a method or property, into
  /// the given declaration context.
  ///
  /// \returns The imported declaration, or null if this declaration could not
  /// be represented in Swift.
  Decl *importMirroredDecl(const clang::NamedDecl *decl, DeclContext *dc,
                           Version version, ProtocolDecl *proto);

  /// \brief Utility function for building simple generic signatures.
  GenericSignature *buildGenericSignature(GenericParamList *genericParams,
                                          DeclContext *dc);

  /// \brief Utility function for building simple generic environments.
  GenericEnvironment *buildGenericEnvironment(GenericParamList *genericParams,
                                              DeclContext *dc);

  /// \brief Import the given Clang declaration context into Swift.
  ///
  /// Usually one will use \c importDeclContextOf instead.
  ///
  /// \returns The imported declaration context, or null if it could not
  /// be converted.
  DeclContext *importDeclContextImpl(const clang::DeclContext *dc);

  /// \brief Import the declaration context of a given Clang declaration into
  /// Swift.
  ///
  /// \param context The effective context as determined by importFullName.
  ///
  /// \returns The imported declaration context, or null if it could not
  /// be converted.
  DeclContext *importDeclContextOf(const clang::Decl *D,
                                   EffectiveClangContext context);

  /// \brief Create a new named constant with the given value.
  ///
  /// \param name The name of the constant.
  /// \param dc The declaration context into which the name will be introduced.
  /// \param type The type of the named constant.
  /// \param value The value of the named constant.
  /// \param convertKind How to convert the constant to the given type.
  /// \param isStatic Whether the constant should be a static member of \p dc.
  ValueDecl *createConstant(Identifier name, DeclContext *dc,
                            Type type, const clang::APValue &value,
                            ConstantConvertKind convertKind,
                            bool isStatic,
                            ClangNode ClangN);

  /// \brief Create a new named constant with the given value.
  ///
  /// \param name The name of the constant.
  /// \param dc The declaration context into which the name will be introduced.
  /// \param type The type of the named constant.
  /// \param value The value of the named constant.
  /// \param convertKind How to convert the constant to the given type.
  /// \param isStatic Whether the constant should be a static member of \p dc.
  ValueDecl *createConstant(Identifier name, DeclContext *dc,
                            Type type, StringRef value,
                            ConstantConvertKind convertKind,
                            bool isStatic,
                            ClangNode ClangN);

  /// \brief Create a new named constant using the given expression.
  ///
  /// \param name The name of the constant.
  /// \param dc The declaration context into which the name will be introduced.
  /// \param type The type of the named constant.
  /// \param valueExpr An expression to use as the value of the constant.
  /// \param convertKind How to convert the constant to the given type.
  /// \param isStatic Whether the constant should be a static member of \p dc.
  ValueDecl *createConstant(Identifier name, DeclContext *dc,
                            Type type, Expr *valueExpr,
                            ConstantConvertKind convertKind,
                            bool isStatic,
                            ClangNode ClangN);

  /// Determine whether the given declaration is considered
  /// 'unavailable' in Swift.
  bool isUnavailableInSwift(const clang::Decl *decl) {
    return importer::isUnavailableInSwift(
        decl, platformAvailability, SwiftContext.LangOpts.EnableObjCInterop);
  }

  /// \brief Add "Unavailable" annotation to the swift declaration.
  void markUnavailable(ValueDecl *decl, StringRef unavailabilityMsg);

  /// \brief Create a decl with error type and an "unavailable" attribute on it
  /// with the specified message.
  ValueDecl *createUnavailableDecl(Identifier name, DeclContext *dc,
                                   Type type, StringRef UnavailableMessage,
                                   bool isStatic, ClangNode ClangN);

  /// \brief Retrieve the standard library module.
  ModuleDecl *getStdlibModule();

  /// \brief Retrieve the named module.
  ///
  /// \param name The name of the module.
  ///
  /// \returns The named module, or null if the module has not been imported.
  ModuleDecl *getNamedModule(StringRef name);

  /// \brief Returns the "Foundation" module, if it can be loaded.
  ///
  /// After this has been called, the Foundation module will or won't be loaded
  /// into the ASTContext.
  ModuleDecl *tryLoadFoundationModule();

  /// \brief Returns the "SIMD" module, if it can be loaded.
  ///
  /// After this has been called, the SIMD module will or won't be loaded
  /// into the ASTContext.
  ModuleDecl *tryLoadSIMDModule();

  /// \brief Retrieves the Swift wrapper for the given Clang module, creating
  /// it if necessary.
  ClangModuleUnit *getWrapperForModule(const clang::Module *underlying);

  /// \brief Constructs a Swift module for the given Clang module.
  ModuleDecl *finishLoadingClangModule(const clang::Module *clangModule,
                                       bool preferAdapter);

  /// \brief Call finishLoadingClangModule on each deferred import collected
  /// while scanning a bridging header or PCH.
  void handleDeferredImports();

  /// \brief Retrieve the named Swift type, e.g., Int32.
  ///
  /// \param moduleName The name of the module in which the type should occur.
  ///
  /// \param name The name of the type to find.
  ///
  /// \returns The named type, or null if the type could not be found.
  Type getNamedSwiftType(StringRef moduleName, StringRef name);

  /// \brief Retrieve the named Swift type, e.g., Int32.
  ///
  /// \param module The module in which the type should occur.
  ///
  /// \param name The name of the type to find.
  ///
  /// \returns The named type, or null if the type could not be found.
  Type getNamedSwiftType(ModuleDecl *module, StringRef name);

  /// \brief Retrieve a specialization of the named Swift type, e.g.,
  /// UnsafeMutablePointer<T>.
  ///
  /// \param module The name of the module in which the type should occur.
  ///
  /// \param name The name of the type to find.
  ///
  /// \param args The arguments to use in the specialization.
  ///
  /// \returns The named type, or null if the type could not be found.
  Type getNamedSwiftTypeSpecialization(ModuleDecl *module, StringRef name,
                                       ArrayRef<Type> args);

  /// \brief Retrieve the NSObject type.
  Type getNSObjectType();

  /// \brief Retrieve the NSObject protocol type.
  Type getNSObjectProtocolType();

  /// \brief Retrieve the NSCopying protocol type.
  Type getNSCopyingType();

  /// \brief Retrieve a sugared referenece to the given (imported) type.
  Type getSugaredTypeReference(TypeDecl *type);

  /// \brief Determines whether the given type matches an implicit type
  /// bound of "Hashable", which is used to validate NSDictionary/NSSet.
  bool matchesHashableBound(Type type);

  /// \brief Determines whether the type declared by the given declaration
  /// is over-aligned.
  bool isOverAligned(const clang::TypeDecl *typeDecl);
  bool isOverAligned(clang::QualType type);

  /// \brief Look up and attempt to import a Clang declaration with
  /// the given name.
  Decl *importDeclByName(StringRef name);

  /// \brief Import the given Clang type into Swift.
  ///
  /// \param type The Clang type to import.
  ///
  /// \param kind A classification of the immediate context in which this type
  ///   will be used. Different contexts result in the type being imported
  ///   differently; for example, CF types are normally considered Unmanaged,
  ///   but in parameter position they are known to always be passed at +0.
  ///   See also the \p topLevelBridgeability parameter.
  ///
  /// \param allowNSUIntegerAsInt If true, NSUInteger will be imported as Int
  ///   in certain contexts. If false, it will always be imported as UInt.
  ///
  /// \param topLevelBridgeability A classification of the top-level context in
  ///   which this type will be used. This and \p kind are used together to
  ///   determine whether a type can be imported in a more Swifty way than
  ///   a naive translation of its C type. Full bridgeability requires that SIL
  ///   can get back to the original Clang type if it needs to, which implies
  ///   that this type is part of a top-level declaration where we do bridging.
  ///   Without full bridgeability, we can still do some Swifty importing (e.g.
  ///   mapping NSString to String) if we're in an immediate context \p kind
  ///   that allows bridging, but only in cases where Swift's default mapping
  ///   "back" to C is the correct one. If the original type has something
  ///   funny going on, we either have to use a less lossy version of the type
  ///   (ObjCBool rather than Bool) or refuse to import it at all (a block with
  ///   the \c ns_returns_retained attribute).
  ///
  /// \param optional If the imported type was a pointer-like type in C, this
  ///   optionality is applied to the resulting Swift type.
  ///
  /// \param resugarNSErrorPointer If true, Objective-C's `NSError **` is
  ///   imported as Foundation.NSErrorPointer rather than
  ///   AutoreleasingUnsafeMutablePointer<...>. This is usually desirable
  ///   behavior, but isn't necessary when we use Swift's \c throws anyway.
  ///   Strictly speaking, though, this is a hack used to break cyclic
  ///   dependencies.
  ///
  /// \returns An ImportedType value which holds the imported type. If
  ///          this type is an Optional, it also has a flag which
  ///          indicates if the Optional is implicitly unwrapped. If
  ///          the type cannot be represented in Swift, then the type
  ///          field will be null.
  ImportedType
  importType(clang::QualType type, ImportTypeKind kind,
             bool allowNSUIntegerAsInt, Bridgeability topLevelBridgeability,
             OptionalTypeKind optional = OTK_ImplicitlyUnwrappedOptional,
             bool resugarNSErrorPointer = true);

  /// \brief Import the given Clang type into Swift.
  ///
  /// For a description of parameters, see importType(). This differs
  /// only in that it returns a Type rather than ImportedType, which
  /// means that we do not retain the information of whether the type
  /// returned might be an implicitly unwrapped optional.
  ///
  /// \returns The imported type, or null if this type could not be
  ///   represented in Swift.
  Type importTypeIgnoreIUO(
      clang::QualType type, ImportTypeKind kind, bool allowNSUIntegerAsInt,
      Bridgeability topLevelBridgeability,
      OptionalTypeKind optional = OTK_ImplicitlyUnwrappedOptional,
      bool resugarNSErrorPointer = true);

  /// \brief Import the given Clang type into Swift, returning the
  /// Swift parameters and result type and whether we should treat it
  /// as an optional that is implicitly unwrapped.
  ///
  /// The parameters are returned via \c parameterList, and the result type is
  /// the return type of this method.
  ///
  /// \returns A pair of the imported result type and whether we should treat
  /// it as an optional that is implicitly unwrapped. The returned
  /// type is null if it cannot be represented in Swift.

  /// \brief Import the given function type.
  ///
  /// This routine should be preferred when importing function types for
  /// which we have actual function parameters, e.g., when dealing with a
  /// function declaration, because it produces a function type whose input
  /// tuple has argument names.
  ///
  /// \param dc The context the function is being imported into.
  /// \param clangDecl The underlying declaration, if any; should only be
  ///   considered for any attributes it might carry.
  /// \param params The parameter types to the function.
  /// \param isVariadic Whether the function is variadic.
  /// \param isFromSystemModule Whether to apply special rules that only apply
  ///   to system APIs.
  /// \param name The name of the function.
  /// \param[out] parameterList The parameters visible inside the function body.
  ImportedType importFunctionType(DeclContext *dc,
                                  const clang::FunctionDecl *clangDecl,
                                  ArrayRef<const clang::ParmVarDecl *> params,
                                  bool isVariadic, bool isFromSystemModule,
                                  DeclName name, ParameterList *&parameterList);

  /// \brief Import the given function return type.
  ///
  /// \param dc The context the function is being imported into.
  /// \param clangDecl The underlying declaration, if any; should only be
  ///   considered for any attributes it might carry.
  /// \param allowNSUIntegerAsInt If true, NSUInteger will be imported as Int
  ///        in certain contexts. If false, it will always be imported as UInt.
  ///
  /// \returns the imported function return type, or null if the type cannot be
  /// imported.
  ImportedType importFunctionReturnType(DeclContext *dc,
                                        const clang::FunctionDecl *clangDecl,
                                        bool allowNSUIntegerAsInt);

  /// \brief Import the parameter list for a function
  ///
  /// \param clangDecl The underlying declaration, if any; should only be
  ///   considered for any attributes it might carry.
  /// \param params The parameter types to the function.
  /// \param isVariadic Whether the function is variadic.
  /// \param allowNSUIntegerAsInt If true, NSUInteger will be imported as Int
  ///        in certain contexts. If false, it will always be imported as UInt.
  /// \param argNames The argument names
  ///
  /// \returns The imported parameter list on success, or null on failure
  ParameterList *
  importFunctionParameterList(DeclContext *dc,
                              const clang::FunctionDecl *clangDecl,
                              ArrayRef<const clang::ParmVarDecl *> params,
                              bool isVariadic, bool allowNSUIntegerAsInt,
                              ArrayRef<Identifier> argNames);

  ImportedType importPropertyType(const clang::ObjCPropertyDecl *clangDecl,
                                  bool isFromSystemModule);

  /// Attempt to infer a default argument for a parameter with the
  /// given Clang \c type, \c baseName, and optionality.
  static DefaultArgumentKind
  inferDefaultArgument(clang::QualType type, OptionalTypeKind clangOptionality,
                       DeclBaseName baseName, unsigned numParams,
                       StringRef argumentLabel, bool isFirstParameter,
                       bool isLastParameter, importer::NameImporter &);

  /// Import the parameter and return types of an Objective-C method.
  ///
  /// The parameters are returned via \c bodyParams, and the result type is
  /// the return type of this method.
  ///
  /// Note that this is not appropriate to use for property accessor methods.
  /// Use #importAccessorMethodType instead.
  ///
  /// \param dc The context the method is being imported into.
  /// \param clangDecl The underlying declaration.
  /// \param params The parameter types to the function. Note that this may not
  ///   include all parameters defined on the ObjCMethodDecl.
  /// \param isVariadic Whether the function is variadic.
  /// \param isFromSystemModule Whether to apply special rules that only apply
  ///   to system APIs.
  /// \param[out] bodyParams The patterns visible inside the function body.
  /// \param importedName How to import the name of the method.
  /// \param[out] errorConvention Whether and how the method throws NSErrors.
  /// \param kind Controls whether we're building a type for a method that
  ///   needs special handling.
  ///
  /// \returns the imported result type, or null if the type cannot be
  /// imported.
  ImportedType
  importMethodType(const DeclContext *dc,
                   const clang::ObjCMethodDecl *clangDecl,
                   ArrayRef<const clang::ParmVarDecl *> params, bool isVariadic,
                   bool isFromSystemModule, ParameterList **bodyParams,
                   importer::ImportedName importedName,
                   Optional<ForeignErrorConvention> &errorConvention,
                   SpecialMethodKind kind);

  /// Import the type of an Objective-C method that will be imported as an
  /// accessor for \p property.
  ///
  /// The parameters are returned via \c bodyParams, and the result type is
  /// the return type of this method.
  ///
  /// \param dc The context the method is being imported into.
  /// \param property The property the method will be an accessor for.
  /// \param clangDecl The underlying declaration.
  /// \param isFromSystemModule Whether to apply special rules that only apply
  ///   to system APIs.
  /// \param importedName How to import the name of the method. This is still
  ///   important to satisfy the AST verifier, even though the method is an
  ///   accessor.
  /// \param[out] params The patterns visible inside the function body.
  ///
  /// \returns the imported result type, or null if the type cannot be
  /// imported.
  ImportedType importAccessorMethodType(const DeclContext *dc,
                                        const clang::ObjCPropertyDecl *property,
                                        const clang::ObjCMethodDecl *clangDecl,
                                        bool isFromSystemModule,
                                        importer::ImportedName importedName,
                                        ParameterList **params);

  /// \brief Determine whether the given typedef-name is "special", meaning
  /// that it has performed some non-trivial mapping of its underlying type
  /// based on the name of the typedef.
  Optional<MappedTypeNameKind>
  getSpecialTypedefKind(clang::TypedefNameDecl *decl);

  /// \brief Look up a name, accepting only typedef results.
  const clang::TypedefNameDecl *lookupTypedef(clang::DeclarationName);

  /// \brief Return whether a global of the given type should be imported as a
  /// 'let' declaration as opposed to 'var'.
  bool shouldImportGlobalAsLet(clang::QualType type);

  LazyResolver *getTypeResolver() const {
    return typeResolver.getPointer();
  }
  void setTypeResolver(LazyResolver *newResolver) {
    assert((!typeResolver.getPointer() || !newResolver) &&
           "already have a type resolver");
    typeResolver.setPointerAndInt(newResolver, true);
  }
  bool hasBegunTypeChecking() const { return typeResolver.getInt(); }
  bool hasFinishedTypeChecking() const {
    return hasBegunTypeChecking() && !getTypeResolver();
  }

  /// Allocate a new delayed conformance ID with the given set of
  /// conformances.
  unsigned allocateDelayedConformance(
             SmallVector<ProtocolConformance *, 4> &&conformances) {
    unsigned id = NextDelayedConformanceID++;
    DelayedConformances[id] = std::move(conformances);
    return id;
  }

  /// Take the delayed conformances associated with the given id.
  SmallVector<ProtocolConformance *, 4> takeDelayedConformance(unsigned id) {
    auto conformances = DelayedConformances.find(id);
    SmallVector<ProtocolConformance *, 4> result
      = std::move(conformances->second);
    DelayedConformances.erase(conformances);
    return result;
  }

  /// Record the set of imported protocols for the given declaration,
  /// to be used by member loading.
  ///
  /// FIXME: This is all a hack; we should have lazier deserialization
  /// of protocols separate from their conformances.
  void recordImportedProtocols(const Decl *decl,
                               ArrayRef<ProtocolDecl *> protocols) {
    if (protocols.empty())
      return;

    auto &recorded = ImportedProtocols[decl];
    recorded.insert(recorded.end(), protocols.begin(), protocols.end());
  }

  /// Retrieve the imported protocols for the given declaration.
  SmallVector<ProtocolDecl *, 4> takeImportedProtocols(const Decl *decl) {
    SmallVector<ProtocolDecl *, 4> result;

    auto known = ImportedProtocols.find(decl);
    if (known != ImportedProtocols.end()) {
      result = std::move(known->second);
      ImportedProtocols.erase(known);
    }

    return result;
  }

  virtual void
  loadAllMembers(Decl *D, uint64_t unused) override;

  virtual Optional<TinyPtrVector<ValueDecl *>>
  loadNamedMembers(const IterableDeclContext *IDC, DeclBaseName N,
                   uint64_t contextData) override;

private:
  void
  loadAllMembersOfObjcContainer(Decl *D,
                                const clang::ObjCContainerDecl *objcContainer);
  void collectMembersToAdd(const clang::ObjCContainerDecl *objcContainer,
                           Decl *D, DeclContext *DC,
                           SmallVectorImpl<Decl *> &members);
  void insertMembersAndAlternates(const clang::NamedDecl *nd,
                                  SmallVectorImpl<Decl *> &members);
  void loadAllMembersIntoExtension(Decl *D, uint64_t extra);

  /// Imports \p decl under \p nameVersion with the name \p newName, and adds
  /// it and its alternates to \p ext.
  ///
  /// \returns true if \p decl was successfully imported, whether or not it was
  /// ultimately added to \p ext. This matches the behavior of
  /// forEachDistinctName's callback.
  bool addMemberAndAlternatesToExtension(
      clang::NamedDecl *decl, importer::ImportedName newName,
      importer::ImportNameVersion nameVersion, ExtensionDecl *ext);

public:
  void
  loadAllConformances(
    const Decl *D, uint64_t contextData,
    SmallVectorImpl<ProtocolConformance *> &Conformances) override;

  void finishNormalConformance(NormalProtocolConformance *conformance,
                               uint64_t unused) override;
  
  /// Returns the default definition type for \p ATD.
  TypeLoc loadAssociatedTypeDefault(const AssociatedTypeDecl *ATD,
                                            uint64_t contextData) override {
    llvm_unreachable("unimplemented for ClangImporter");
  }
  
  /// Returns the generic environment.
  virtual GenericEnvironment *loadGenericEnvironment(const DeclContext *decl,
                                                     uint64_t contextData) override {
    llvm_unreachable("unimplemented for ClangImporter");
  }

  template <typename DeclTy, typename ...Targs>
  DeclTy *createDeclWithClangNode(ClangNode ClangN, AccessLevel access,
                                  Targs &&... Args) {
    assert(ClangN);
    void *DeclPtr = allocateMemoryForDecl<DeclTy>(SwiftContext, sizeof(DeclTy),
                                                  true);
    auto D = ::new (DeclPtr) DeclTy(std::forward<Targs>(Args)...);
    D->setClangNode(ClangN);
    D->setEarlyAttrValidation(true);
    D->setAccess(access);
    if (auto ASD = dyn_cast<AbstractStorageDecl>(D))
      ASD->setSetterAccess(access);
    // All imported decls are constructed fully validated.
    D->setValidationToChecked();
    if (auto AFD = dyn_cast<AbstractFunctionDecl>(static_cast<Decl *>(D)))
      AFD->setNeedsNewVTableEntry(false);
    return D;
  }

  /// Find the lookup table that corresponds to the given Clang module.
  ///
  /// \param clangModule The module, or null to indicate that we're talking
  /// about the directly-parsed headers.
  SwiftLookupTable *findLookupTable(const clang::Module *clangModule);

  /// Visit each of the lookup tables in some deterministic order.
  ///
  /// \param fn Invoke the given visitor for each table. If the
  /// visitor returns true, stop early.
  ///
  /// \returns \c true if the \c visitor ever returns \c true, \c
  /// false otherwise.
  bool forEachLookupTable(llvm::function_ref<bool(SwiftLookupTable &table)> fn);

  /// Look for namespace-scope values with the given name in the given
  /// Swift lookup table.
  void lookupValue(SwiftLookupTable &table, DeclName name,
                   VisibleDeclConsumer &consumer);

  /// Look for namespace-scope values in the given Swift lookup table.
  void lookupVisibleDecls(SwiftLookupTable &table,
                          VisibleDeclConsumer &consumer);

  /// Look for Objective-C members with the given name in the given
  /// Swift lookup table.
  void lookupObjCMembers(SwiftLookupTable &table, DeclName name,
                         VisibleDeclConsumer &consumer);

  /// Look for all Objective-C members in the given Swift lookup table.
  void lookupAllObjCMembers(SwiftLookupTable &table,
                            VisibleDeclConsumer &consumer);

  /// Determine the effective Clang context for the given Swift nominal type.
  EffectiveClangContext
  getEffectiveClangContext(const NominalTypeDecl *nominal);

  /// Attempts to import the name of \p decl with each possible
  /// ImportNameVersion. \p action will be called with each unique name.
  ///
  /// In this case, "unique" means either the full name is distinct or the
  /// effective context is distinct. This method does not attempt to handle
  /// "unresolved" contexts in any special way---if one name references a
  /// particular Clang declaration and the other has an unresolved context that
  /// will eventually reference that declaration, the contexts will still be
  /// considered distinct.
  ///
  /// If \p action returns false, the current name will \e not be added to the
  /// set of seen names.
  ///
  /// The active name is always first, followed by the other names in the order
  /// of ImportNameVersion::forEachOtherImportNameVersion.
  void forEachDistinctName(
      const clang::NamedDecl *decl,
      llvm::function_ref<bool(importer::ImportedName,
                              importer::ImportNameVersion)> action) {
    getNameImporter().forEachDistinctImportName(decl, CurrentVersion, action);
  }

  /// Dump the Swift-specific name lookup tables we generate.
  void dumpSwiftLookupTables();

  void setSinglePCHImport(Optional<std::string> PCHFilename) {
    if (PCHFilename.hasValue()) {
      assert(llvm::sys::path::extension(PCHFilename.getValue())
                 .endswith(file_types::getExtension(file_types::TY_PCH)) &&
             "Single PCH imported filename doesn't have .pch extension!");
    }
    SinglePCHImport = PCHFilename;
  }

  /// If there was is a single .pch bridging header without other imported
  /// files, we can provide the PCH filename for declaration caching,
  /// especially in code completion.
  StringRef getSinglePCHImport() const {
    if (SinglePCHImport.hasValue())
      return *SinglePCHImport;
    return StringRef();
  }
};

namespace importer {

/// Whether we should suppress the import of the given Clang declaration.
bool shouldSuppressDeclImport(const clang::Decl *decl);

/// Identifies certain UIKit constants that used to have overlay equivalents,
/// but are now renamed using the swift_name attribute.
bool isSpecialUIKitStructZeroProperty(const clang::NamedDecl *decl);

/// Finds a particular kind of nominal by looking through typealiases.
template <typename T>
static T *dynCastIgnoringCompatibilityAlias(Decl *D) {
  static_assert(std::is_base_of<NominalTypeDecl, T>::value,
                "only meant for use with NominalTypeDecl and subclasses");
  if (auto *alias = dyn_cast_or_null<TypeAliasDecl>(D)) {
    if (!alias->isCompatibilityAlias())
      return nullptr;
    D = alias->getDeclaredInterfaceType()->getAnyNominal();
  }
  return dyn_cast_or_null<T>(D);
}

/// Finds a particular kind of nominal by looking through typealiases.
template <typename T>
static T *castIgnoringCompatibilityAlias(Decl *D) {
  static_assert(std::is_base_of<NominalTypeDecl, T>::value,
                "only meant for use with NominalTypeDecl and subclasses");
  if (auto *alias = dyn_cast_or_null<TypeAliasDecl>(D)) {
    assert(alias->isCompatibilityAlias() &&
           "non-compatible typealias found where nominal was expected");
    D = alias->getDeclaredInterfaceType()->getAnyNominal();
  }
  return cast_or_null<T>(D);
}

class SwiftNameLookupExtension : public clang::ModuleFileExtension {
  std::unique_ptr<SwiftLookupTable> &pchLookupTable;
  LookupTableMap &lookupTables;
  ASTContext &swiftCtx;
  const PlatformAvailability &availability;
  const bool inferImportAsMember;

public:
  SwiftNameLookupExtension(std::unique_ptr<SwiftLookupTable> &pchLookupTable,
                           LookupTableMap &tables, ASTContext &ctx,
                           const PlatformAvailability &avail, bool inferIAM)
      : pchLookupTable(pchLookupTable), lookupTables(tables), swiftCtx(ctx),
        availability(avail), inferImportAsMember(inferIAM) {}

  clang::ModuleFileExtensionMetadata getExtensionMetadata() const override;
  llvm::hash_code hashExtension(llvm::hash_code code) const override;

  std::unique_ptr<clang::ModuleFileExtensionWriter>
  createExtensionWriter(clang::ASTWriter &writer) override;

  std::unique_ptr<clang::ModuleFileExtensionReader>
  createExtensionReader(const clang::ModuleFileExtensionMetadata &metadata,
                        clang::ASTReader &reader,
                        clang::serialization::ModuleFile &mod,
                        const llvm::BitstreamCursor &stream) override;
};

}
}

#endif
