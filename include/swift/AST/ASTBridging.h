//===--- ASTBridging.h - header for the swift SILBridging module ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ASTBRIDGING_H
#define SWIFT_AST_ASTBRIDGING_H

// Do not add other C++/llvm/swift header files here!
// Function implementations should be placed into ASTBridging.cpp and required header files should be added there.
//
// Pure bridging mode does not permit including any C++/llvm/swift headers.
// See also the comments for `BRIDGING_MODE` in the top-level CMakeLists.txt file.
//
#include "swift/Basic/BasicBridging.h"

#ifdef USED_IN_CPP_SOURCE
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#endif

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

namespace llvm {
template<typename T> class ArrayRef;
}

namespace swift {
class Argument;
class ASTContext;
struct ASTNode;
class DeclAttributes;
class DeclBaseName;
class DeclNameLoc;
class DeclNameRef;
class DiagnosticArgument;
class DiagnosticEngine;
class Fingerprint;
class Identifier;
class IfConfigClauseRangeInfo;
struct LabeledStmtInfo;
enum class MacroRole : uint32_t;
class MacroIntroducedDeclName;
enum class MacroIntroducedDeclNameKind;
enum class ParamSpecifier : uint8_t;
class ProtocolConformanceRef;
class RegexLiteralPatternFeature;
class RegexLiteralPatternFeatureKind;
class Type;
class CanType;
class TypeBase;
class StmtConditionElement;
class SubstitutionMap;
}

struct BridgedASTType;
class BridgedCanType;
class BridgedASTContext;
struct BridgedSubstitutionMap;
class BridgedParameterList;

//===----------------------------------------------------------------------===//
// MARK: Identifier
//===----------------------------------------------------------------------===//

class BridgedIdentifier {
public:
  SWIFT_UNAVAILABLE("Use '.raw' instead")
  const void *_Nullable Raw;

  BridgedIdentifier() : Raw(nullptr) {}

  SWIFT_NAME("init(raw:)")
  BridgedIdentifier(const void *_Nullable raw) : Raw(raw) {}

  BRIDGED_INLINE BridgedIdentifier(swift::Identifier ident);

  BRIDGED_INLINE swift::Identifier unbridged() const;
};

SWIFT_NAME("getter:BridgedIdentifier.raw(self:)")
inline const void *_Nullable BridgedIdentifier_raw(BridgedIdentifier ident) {
  return ident.Raw;
}

SWIFT_NAME("getter:BridgedIdentifier.isOperator(self:)")
BRIDGED_INLINE bool BridgedIdentifier_isOperator(const BridgedIdentifier);

struct BridgedLocatedIdentifier {
  SWIFT_NAME("name")
  BridgedIdentifier Name;

  SWIFT_NAME("nameLoc")
  BridgedSourceLoc NameLoc;
};

class BridgedDeclBaseName {
  BridgedIdentifier Ident;

public:
  BRIDGED_INLINE BridgedDeclBaseName(swift::DeclBaseName baseName);

  BRIDGED_INLINE swift::DeclBaseName unbridged() const;
};

SWIFT_NAME("BridgedDeclBaseName.createConstructor()")
BridgedDeclBaseName BridgedDeclBaseName_createConstructor();

SWIFT_NAME("BridgedDeclBaseName.createDestructor()")
BridgedDeclBaseName BridgedDeclBaseName_createDestructor();

SWIFT_NAME("BridgedDeclBaseName.createSubscript()")
BridgedDeclBaseName BridgedDeclBaseName_createSubscript();

SWIFT_NAME("BridgedDeclBaseName.createIdentifier(_:)")
BridgedDeclBaseName
BridgedDeclBaseName_createIdentifier(BridgedIdentifier identifier);

class BridgedDeclNameRef {
  void *_Nonnull opaque;

public:
  BRIDGED_INLINE BridgedDeclNameRef();

  BRIDGED_INLINE BridgedDeclNameRef(swift::DeclNameRef name);

  BRIDGED_INLINE swift::DeclNameRef unbridged() const;
};

SWIFT_NAME("BridgedDeclNameRef.createParsed(_:baseName:argumentLabels:)")
BridgedDeclNameRef
BridgedDeclNameRef_createParsed(BridgedASTContext cContext,
                                BridgedDeclBaseName cBaseName,
                                BridgedArrayRef cLabels);

SWIFT_NAME("BridgedDeclNameRef.createParsed(_:)")
BridgedDeclNameRef
BridgedDeclNameRef_createParsed(BridgedDeclBaseName cBaseName);

class BridgedDeclNameLoc {
  const void *_Nullable LocationInfo;
  size_t NumArgumentLabels;

public:
  BridgedDeclNameLoc() : LocationInfo(nullptr), NumArgumentLabels(0) {}

  BRIDGED_INLINE BridgedDeclNameLoc(swift::DeclNameLoc loc);

  BRIDGED_INLINE swift::DeclNameLoc unbridged() const;
};

SWIFT_NAME("BridgedDeclNameLoc.createParsed(_:baseNameLoc:lParenLoc:"
           "argumentLabelLocs:rParenLoc:)")
BridgedDeclNameLoc BridgedDeclNameLoc_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cBaseNameLoc,
    BridgedSourceLoc cLParenLoc, BridgedArrayRef cLabelLocs,
    BridgedSourceLoc cRParenLoc);

SWIFT_NAME("BridgedDeclNameLoc.createParsed(_:)")
BridgedDeclNameLoc
BridgedDeclNameLoc_createParsed(BridgedSourceLoc cBaseNameLoc);

//===----------------------------------------------------------------------===//
// MARK: ASTContext
//===----------------------------------------------------------------------===//

class BridgedASTContext {
  swift::ASTContext * _Nonnull Ctx;

public:
  SWIFT_UNAVAILABLE("Use init(raw:) instead")
  BRIDGED_INLINE BridgedASTContext(swift::ASTContext &ctx);

  SWIFT_UNAVAILABLE("Use '.raw' instead")
  BRIDGED_INLINE swift::ASTContext &unbridged() const;
};

SWIFT_NAME("getter:BridgedASTContext.raw(self:)")
BRIDGED_INLINE
void * _Nonnull BridgedASTContext_raw(BridgedASTContext bridged);

SWIFT_NAME("BridgedASTContext.init(raw:)")
BRIDGED_INLINE
BridgedASTContext BridgedASTContext_fromRaw(void * _Nonnull ptr);

SWIFT_NAME("BridgedASTContext.allocate(self:size:alignment:)")
BRIDGED_INLINE
void *_Nullable BridgedASTContext_allocate(BridgedASTContext bridged,
                                           size_t size, size_t alignment);

SWIFT_NAME("BridgedASTContext.allocateCopy(self:string:)")
BRIDGED_INLINE
BridgedStringRef
BridgedASTContext_allocateCopyString(BridgedASTContext cContext,
                                     BridgedStringRef cStr);

SWIFT_NAME("BridgedASTContext.getIdentifier(self:_:)")
BridgedIdentifier BridgedASTContext_getIdentifier(BridgedASTContext cContext,
                                                  BridgedStringRef cStr);

SWIFT_NAME("BridgedASTContext.langOptsHasFeature(self:_:)")
bool BridgedASTContext_langOptsHasFeature(BridgedASTContext cContext,
                                          BridgedFeature feature);

SWIFT_NAME("getter:BridgedASTContext.majorLanguageVersion(self:)")
unsigned BridgedASTContext_majorLanguageVersion(BridgedASTContext cContext);

SWIFT_NAME("BridgedASTContext.langOptsCustomConditionSet(self:_:)")
bool BridgedASTContext_langOptsCustomConditionSet(BridgedASTContext cContext,
                                                  BridgedStringRef cName);

SWIFT_NAME("BridgedASTContext.langOptsHasFeatureNamed(self:_:)")
bool BridgedASTContext_langOptsHasFeatureNamed(BridgedASTContext cContext,
                                               BridgedStringRef cName);

SWIFT_NAME("BridgedASTContext.langOptsHasAttributeNamed(self:_:)")
bool BridgedASTContext_langOptsHasAttributeNamed(BridgedASTContext cContext,
                                                 BridgedStringRef cName);

SWIFT_NAME("BridgedASTContext.langOptsIsActiveTargetOS(self:_:)")
bool BridgedASTContext_langOptsIsActiveTargetOS(BridgedASTContext cContext,
                                                BridgedStringRef cName);

SWIFT_NAME("BridgedASTContext.langOptsIsActiveTargetArchitecture(self:_:)")
bool BridgedASTContext_langOptsIsActiveTargetArchitecture(BridgedASTContext cContext,
                                                          BridgedStringRef cName);

SWIFT_NAME("BridgedASTContext.langOptsIsActiveTargetEnvironment(self:_:)")
bool BridgedASTContext_langOptsIsActiveTargetEnvironment(BridgedASTContext cContext,
                                                         BridgedStringRef cName);

SWIFT_NAME("BridgedASTContext.langOptsIsActiveTargetRuntime(self:_:)")
bool BridgedASTContext_langOptsIsActiveTargetRuntime(BridgedASTContext cContext,
                                                     BridgedStringRef cName);

SWIFT_NAME("BridgedASTContext.langOptsIsActiveTargetPtrAuth(self:_:)")
bool BridgedASTContext_langOptsIsActiveTargetPtrAuth(BridgedASTContext cContext,
                                                     BridgedStringRef cName);

SWIFT_NAME("getter:BridgedASTContext.langOptsTargetPointerBitWidth(self:)")
unsigned BridgedASTContext_langOptsTargetPointerBitWidth(BridgedASTContext cContext);

SWIFT_NAME("BridgedASTContext.langOptsGetTargetAtomicBitWidths(self:_:)")
SwiftInt BridgedASTContext_langOptsGetTargetAtomicBitWidths(BridgedASTContext cContext,
                                                      SwiftInt* _Nullable * _Nonnull cComponents);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedEndianness : size_t {
  EndianLittle,
  EndianBig,
};

SWIFT_NAME("getter:BridgedASTContext.langOptsTargetEndianness(self:)")
BridgedEndianness BridgedASTContext_langOptsTargetEndianness(BridgedASTContext cContext);

SWIFT_NAME("BridgedASTContext.langOptsGetLanguageVersion(self:_:)")
SwiftInt BridgedASTContext_langOptsGetLanguageVersion(BridgedASTContext cContext,
                                                      SwiftInt* _Nullable * _Nonnull cComponents);

SWIFT_NAME("BridgedASTContext.langOptsGetCompilerVersion(self:_:)")
SwiftInt BridgedASTContext_langOptsGetCompilerVersion(BridgedASTContext cContext,
                                                      SwiftInt* _Nullable * _Nonnull cComponents);

/* Deallocate an array of Swift int values that was allocated in C++. */
void deallocateIntBuffer(SwiftInt * _Nullable cComponents);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedCanImportVersion : size_t {
  CanImportUnversioned,
  CanImportVersion,
  CanImportUnderlyingVersion,
};

SWIFT_NAME("BridgedASTContext.canImport(self:importPath:location:versionKind:versionComponents:numVersionComponents:)")
bool BridgedASTContext_canImport(BridgedASTContext cContext,
                                 BridgedStringRef importPath,
                                 BridgedSourceLoc canImportLoc,
                                 BridgedCanImportVersion versionKind,
                                 const SwiftInt * _Nullable versionComponents,
                                 SwiftInt numVersionComponents);

//===----------------------------------------------------------------------===//
// MARK: AST nodes
//===----------------------------------------------------------------------===//

enum ENUM_EXTENSIBILITY_ATTR(open) ASTNodeKind : size_t {
  ASTNodeKindExpr,
  ASTNodeKindStmt,
  ASTNodeKindDecl
};

void registerBridgedDecl(BridgedStringRef bridgedClassName, SwiftMetatype metatype);

struct OptionalBridgedDeclObj {
  OptionalSwiftObject obj;
};

struct BridgedDeclObj {
  SwiftObject obj;

#ifdef USED_IN_CPP_SOURCE
  template <class D> D *_Nonnull getAs() const {
    return llvm::cast<D>(static_cast<swift::Decl *>(obj));
  }
  swift::Decl * _Nonnull unbridged() const {
    return getAs<swift::Decl>();
  }
#endif

  BridgedDeclObj(SwiftObject obj) : obj(obj) {}
  BridgedOwnedString getDebugDescription() const;
  BRIDGED_INLINE BridgedSourceLoc getLoc() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedStringRef Type_getName() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedStringRef Value_getUserFacingName() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedSourceLoc Value_getNameLoc() const;
  BRIDGED_INLINE bool Value_isObjC() const;
  BRIDGED_INLINE bool GenericType_isGenericAtAnyLevel() const;
  BRIDGED_INLINE bool NominalType_isGlobalActor() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE OptionalBridgedDeclObj NominalType_getValueTypeDestructor() const;
  BRIDGED_INLINE bool Struct_hasUnreferenceableStorage() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedASTType Class_getSuperclass() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedDeclObj Class_getDestructor() const;
  BRIDGED_INLINE bool Destructor_isIsolated() const;
};

struct BridgedASTNode {
  SWIFT_NAME("raw")
  void *_Nonnull Raw;

  SWIFT_NAME("kind")
  ASTNodeKind Kind;

  BRIDGED_INLINE swift::ASTNode unbridged() const;
};

// Forward declare the underlying AST node type for each wrapper.
namespace swift {
#define AST_BRIDGING_WRAPPER(Name) class Name;
#include "swift/AST/ASTBridgingWrappers.def"
} // end namespace swift

// Define the bridging wrappers for each AST node.
#define AST_BRIDGING_WRAPPER(Name) BRIDGING_WRAPPER_NONNULL(swift::Name, Name)
#include "swift/AST/ASTBridgingWrappers.def"

// For nullable nodes, also define a nullable variant.
#define AST_BRIDGING_WRAPPER_NULLABLE(Name)                                    \
  BRIDGING_WRAPPER_NULLABLE(swift::Name, Name)
#define AST_BRIDGING_WRAPPER_NONNULL(Name)
#include "swift/AST/ASTBridgingWrappers.def"

// Declare `.asDecl` on each BridgedXXXDecl type, which upcasts a wrapper for
// a Decl subclass to a BridgedDecl.
#define DECL(Id, Parent)                                                       \
  SWIFT_NAME("getter:Bridged" #Id "Decl.asDecl(self:)")                        \
  BridgedDecl Bridged##Id##Decl_asDecl(Bridged##Id##Decl decl);
#define ABSTRACT_DECL(Id, Parent) DECL(Id, Parent)
#include "swift/AST/DeclNodes.def"

// Declare `.asDeclContext` on each BridgedXXXDecl type that's also a
// DeclContext.
#define DECL(Id, Parent)
#define CONTEXT_DECL(Id, Parent)                                               \
  SWIFT_NAME("getter:Bridged" #Id "Decl.asDeclContext(self:)")                 \
  BridgedDeclContext Bridged##Id##Decl_asDeclContext(Bridged##Id##Decl decl);
#define ABSTRACT_CONTEXT_DECL(Id, Parent) CONTEXT_DECL(Id, Parent)
#include "swift/AST/DeclNodes.def"

// Declare `.asStmt` on each BridgedXXXStmt type, which upcasts a wrapper for
// a Stmt subclass to a BridgedStmt.
#define STMT(Id, Parent)                                                       \
  SWIFT_NAME("getter:Bridged" #Id "Stmt.asStmt(self:)")                        \
  BridgedStmt Bridged##Id##Stmt_asStmt(Bridged##Id##Stmt stmt);
#define ABSTRACT_STMT(Id, Parent) STMT(Id, Parent)
#include "swift/AST/StmtNodes.def"

// Declare `.asExpr` on each BridgedXXXExpr type, which upcasts a wrapper for
// a Expr subclass to a BridgedExpr.
#define EXPR(Id, Parent)                                                       \
  SWIFT_NAME("getter:Bridged" #Id "Expr.asExpr(self:)")                        \
  BridgedExpr Bridged##Id##Expr_asExpr(Bridged##Id##Expr expr);
#define ABSTRACT_EXPR(Id, Parent) EXPR(Id, Parent)
#include "swift/AST/ExprNodes.def"

// Declare `.asTypeRepr` on each BridgedXXXTypeRepr type, which upcasts a
// wrapper for a TypeRepr subclass to a BridgedTypeRepr.
#define TYPEREPR(Id, Parent)                                                   \
  SWIFT_NAME("getter:Bridged" #Id "TypeRepr.asTypeRepr(self:)")                \
  BridgedTypeRepr Bridged##Id##TypeRepr_asTypeRepr(                            \
      Bridged##Id##TypeRepr typeRepr);
#define ABSTRACT_TYPEREPR(Id, Parent) TYPEREPR(Id, Parent)
#include "swift/AST/TypeReprNodes.def"

// Declare `.asPattern` on each BridgedXXXPattern type, which upcasts a wrapper
// for a Pattern subclass to a BridgedPattern.
#define PATTERN(Id, Parent)                                                    \
  SWIFT_NAME("getter:Bridged" #Id "Pattern.asPattern(self:)")                  \
  BridgedPattern Bridged##Id##Pattern_asPattern(Bridged##Id##Pattern pattern);
#include "swift/AST/PatternNodes.def"

// Declare `.asDeclAttribute` on each BridgedXXXAttr type, which upcasts a
// wrapper for a DeclAttribute subclass to a BridgedDeclAttribute.
#define SIMPLE_DECL_ATTR(...)
#define DECL_ATTR(_, CLASS, ...)                                               \
  SWIFT_NAME("getter:Bridged" #CLASS "Attr.asDeclAttribute(self:)")            \
  BridgedDeclAttribute Bridged##CLASS##Attr_asDeclAttribute(                   \
      Bridged##CLASS##Attr attr);
#include "swift/AST/DeclAttr.def"

struct BridgedPatternBindingEntry {
  BridgedPattern pattern;
  BridgedSourceLoc equalLoc;
  BridgedNullableExpr init;
  BridgedNullablePatternBindingInitializer initContext;
};

//===----------------------------------------------------------------------===//
// MARK: Diagnostic Engine
//===----------------------------------------------------------------------===//

// NOTE: This must be the same underlying value as C++ 'swift::DiagID' defined
// in 'DiagnosticList.cpp'.
enum ENUM_EXTENSIBILITY_ATTR(open) BridgedDiagID : uint32_t {
#define DIAG(KIND, ID, Group, Options, Text, Signature) BridgedDiagID_##ID,
#include "swift/AST/DiagnosticsAll.def"
};

class BridgedDiagnosticArgument {
  int64_t storage[3];

public:
  BRIDGED_INLINE BridgedDiagnosticArgument(const swift::DiagnosticArgument &arg);
  BRIDGED_INLINE const swift::DiagnosticArgument &unbridged() const;

  BridgedDiagnosticArgument(SwiftInt i);
  BridgedDiagnosticArgument(BridgedStringRef s);
};

class BridgedDiagnosticFixIt {
public:
  int64_t storage[7];

  BridgedDiagnosticFixIt(BridgedSourceLoc start, uint32_t length, BridgedStringRef text);
};

/// Diagnostic severity when reporting diagnostics.
enum ENUM_EXTENSIBILITY_ATTR(open) BridgedDiagnosticSeverity : size_t {
  BridgedFatalError,
  BridgedError,
  BridgedWarning,
  BridgedRemark,
  BridgedNote,
};

class BridgedDiagnostic {
public:
  struct Impl;

  SWIFT_UNAVAILABLE("Unavailable in Swift")
  Impl *_Nonnull Raw;

  SWIFT_UNAVAILABLE("Unavailable in Swift")
  BridgedDiagnostic(Impl *_Nonnull raw) : Raw(raw) {}

  SWIFT_UNAVAILABLE("Unavailable in Swift")
  Impl *_Nonnull unbridged() const { return Raw; }
};

// FIXME: Can we bridge InFlightDiagnostic?
SWIFT_NAME("BridgedDiagnosticEngine.diagnose(self:at:_:_:highlightAt:"
           "highlightLength:fixIts:)")
void BridgedDiagnosticEngine_diagnose(
    BridgedDiagnosticEngine, BridgedSourceLoc loc, BridgedDiagID diagID,
    BridgedArrayRef arguments, BridgedSourceLoc highlightStart,
    uint32_t hightlightLength, BridgedArrayRef fixIts);

SWIFT_NAME("getter:BridgedDiagnosticEngine.hadAnyError(self:)")
bool BridgedDiagnosticEngine_hadAnyError(BridgedDiagnosticEngine);

/// Create a new diagnostic with the given severity, location, and diagnostic
/// text.
///
/// \returns a diagnostic instance that can be extended with additional
/// information and then must be finished via \c SwiftDiagnostic_finish.
SWIFT_NAME("BridgedDiagnostic.init(at:message:severity:engine:)")
BridgedDiagnostic BridgedDiagnostic_create(BridgedSourceLoc cLoc,
                                           BridgedStringRef cText,
                                           BridgedDiagnosticSeverity severity,
                                           BridgedDiagnosticEngine cDiags);

/// Highlight a source range as part of the diagnostic.
SWIFT_NAME("BridgedDiagnostic.highlight(self:start:end:)")
void BridgedDiagnostic_highlight(BridgedDiagnostic cDiag,
                                 BridgedSourceLoc cStartLoc,
                                 BridgedSourceLoc cEndLoc);

/// Add a Fix-It to replace a source range as part of the diagnostic.
SWIFT_NAME("BridgedDiagnostic.fixItReplace(self:start:end:replacement:)")
void BridgedDiagnostic_fixItReplace(BridgedDiagnostic cDiag,
                                    BridgedSourceLoc cStartLoc,
                                    BridgedSourceLoc cEndLoc,
                                    BridgedStringRef cReplaceText);

/// Finish the given diagnostic and emit it.
SWIFT_NAME("BridgedDiagnostic.finish(self:)")
void BridgedDiagnostic_finish(BridgedDiagnostic cDiag);

//===----------------------------------------------------------------------===//
// MARK: DeclContexts
//===----------------------------------------------------------------------===//

SWIFT_NAME("getter:BridgedDeclContext.isLocalContext(self:)")
BRIDGED_INLINE bool
BridgedDeclContext_isLocalContext(BridgedDeclContext cDeclContext);

SWIFT_NAME("getter:BridgedDeclContext.isTypeContext(self:)")
BRIDGED_INLINE bool BridgedDeclContext_isTypeContext(BridgedDeclContext dc);

SWIFT_NAME("getter:BridgedDeclContext.isModuleScopeContext(self:)")
BRIDGED_INLINE bool
BridgedDeclContext_isModuleScopeContext(BridgedDeclContext dc);

SWIFT_NAME("getter:BridgedDeclContext.astContext(self:)")
BRIDGED_INLINE BridgedASTContext
BridgedDeclContext_getASTContext(BridgedDeclContext dc);

SWIFT_NAME("BridgedPatternBindingInitializer.create(declContext:)")
BridgedPatternBindingInitializer
BridgedPatternBindingInitializer_create(BridgedDeclContext cDeclContext);

SWIFT_NAME("getter:BridgedPatternBindingInitializer.asDeclContext(self:)")
BridgedDeclContext BridgedPatternBindingInitializer_asDeclContext(
    BridgedPatternBindingInitializer cInit);

SWIFT_NAME("BridgedCustomAttributeInitializer.create(declContext:)")
BridgedCustomAttributeInitializer
BridgedCustomAttributeInitializer_create(BridgedDeclContext cDeclContext);

SWIFT_NAME("getter:BridgedCustomAttributeInitializer.asDeclContext(self:)")
BridgedDeclContext BridgedCustomAttributeInitializer_asDeclContext(
    BridgedCustomAttributeInitializer cInit);

SWIFT_NAME("getter:BridgedClosureExpr.asDeclContext(self:)")
BridgedDeclContext
BridgedClosureExpr_asDeclContext(BridgedClosureExpr cClosure);

//===----------------------------------------------------------------------===//
// MARK: DeclAttributes
//===----------------------------------------------------------------------===//

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedDeclAttrKind {
#define DECL_ATTR(_, CLASS, ...) BridgedDeclAttrKind##CLASS,
#include "swift/AST/DeclAttr.def"
  BridgedDeclAttrKindNone,
};

SWIFT_NAME("BridgedDeclAttrKind.init(from:)")
BridgedDeclAttrKind BridgedDeclAttrKind_fromString(BridgedStringRef cStr);

struct BridgedDeclAttributes {
  BridgedNullableDeclAttribute chain;

  BridgedDeclAttributes() : chain(nullptr){};

  BRIDGED_INLINE BridgedDeclAttributes(swift::DeclAttributes attrs);

  BRIDGED_INLINE swift::DeclAttributes unbridged() const;
};

SWIFT_NAME("BridgedDeclAttributes.add(self:_:)")
void BridgedDeclAttributes_add(BridgedDeclAttributes *_Nonnull attrs,
                               BridgedDeclAttribute add);

SWIFT_NAME("BridgedDeclAttribute.createSimple(_:kind:atLoc:nameLoc:)")
BridgedDeclAttribute BridgedDeclAttribute_createSimple(
    BridgedASTContext cContext, BridgedDeclAttrKind cKind,
    BridgedSourceLoc cAtLoc, BridgedSourceLoc cNameLoc);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedAccessLevel {
  BridgedAccessLevelPrivate,
  BridgedAccessLevelFilePrivate,
  BridgedAccessLevelInternal,
  BridgedAccessLevelPackage,
  BridgedAccessLevelPublic,
  BridgedAccessLevelOpen,
  BridgedAccessLevelNone,
};

SWIFT_NAME("BridgedAccessControlAttr.createParsed(_:range:accessLevel:)")
BridgedAccessControlAttr
BridgedAccessControlAttr_createParsed(BridgedASTContext cContext,
                                      BridgedSourceRange cRange,
                                      BridgedAccessLevel cAccessLevel);

SWIFT_NAME("BridgedAlignmentAttr.createParsed(_:atLoc:range:value:)")
BridgedAlignmentAttr
BridgedAlignmentAttr_createParsed(BridgedASTContext cContext,
                                  BridgedSourceLoc cAtLoc,
                                  BridgedSourceRange cRange, size_t cValue);

SWIFT_NAME("BridgedAllowFeatureSuppressionAttr.createParsed(_:atLoc:range:inverted:features:)")
BridgedAllowFeatureSuppressionAttr
BridgedAllowFeatureSuppressionAttr_createParsed(
                                  BridgedASTContext cContext,
                                  BridgedSourceLoc cAtLoc,
                                  BridgedSourceRange cRange,
                                  bool inverted,
                                  BridgedArrayRef cFeatures);

SWIFT_NAME("BridgedCDeclAttr.createParsed(_:atLoc:range:name:)")
BridgedCDeclAttr BridgedCDeclAttr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cAtLoc,
                                               BridgedSourceRange cRange,
                                               BridgedStringRef cName);

SWIFT_NAME(
    "BridgedCustomAttr.createParsed(_:atLoc:type:initContext:argumentList:)")
BridgedCustomAttr BridgedCustomAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc, BridgedTypeRepr cType,
    BridgedNullableCustomAttributeInitializer cInitContext,
    BridgedNullableArgumentList cArgumentList);

SWIFT_NAME("BridgedDocumentationAttr.createParsed(_:atLoc:range:metadata:"
           "accessLevel:)")
BridgedDocumentationAttr BridgedDocumentationAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedStringRef cMetadata,
    BridgedAccessLevel cAccessLevel);

SWIFT_NAME(
    "BridgedDynamicReplacementAttr.createParsed(_:atLoc:attrNameLoc:lParenLoc:"
    "replacedFunction:rParenLoc:)")
BridgedDynamicReplacementAttr BridgedDynamicReplacementAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceLoc cAttrNameLoc, BridgedSourceLoc cLParenLoc,
    BridgedDeclNameRef cReplacedFunction, BridgedSourceLoc cRParenLoc);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedEffectsKind {
  BridgedEffectsKindReadNone,
  BridgedEffectsKindReadOnly,
  BridgedEffectsKindReleaseNone,
  BridgedEffectsKindReadWrite,
  BridgedEffectsKindUnspecified,
  BridgedEffectsKindCustom,
};

SWIFT_NAME("BridgedEffectsAttr.createParsed(_:atLoc:range:effectKind:)")
BridgedEffectsAttr BridgedEffectsAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedEffectsKind cEffectKind);

SWIFT_NAME("BridgedEffectsAttr.createParsed(_:atLoc:range:customString:"
           "customStringLoc:)")
BridgedEffectsAttr BridgedEffectsAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedStringRef cCustomString,
    BridgedSourceLoc cCustomStringLoc);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedExclusivityAttrMode {
  BridgedExclusivityAttrModeChecked,
  BridgedExclusivityAttrModeUnchecked,
};

SWIFT_NAME("BridgedExclusivityAttr.createParsed(_:atLoc:range:mode:)")
BridgedExclusivityAttr BridgedExclusivityAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedExclusivityAttrMode cMode);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedExposureKind {
  BridgedExposureKindCxx,
  BridgedExposureKindWasm,
};

SWIFT_NAME("BridgedExposeAttr.createParsed(_:atLoc:range:name:kind:)")
BridgedExposeAttr BridgedExposeAttr_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cAtLoc,
                                                 BridgedSourceRange cRange,
                                                 BridgedStringRef cName,
                                                 BridgedExposureKind cKind);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedExternKind {
  BridgedExternKindC,
  BridgedExternKindWasm,
};

SWIFT_NAME("BridgedExternAttr.createParsed(_:atLoc:range:lParenLoc:rParenLoc:"
           "kind:moduleName:name:)")
BridgedExternAttr BridgedExternAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedSourceLoc cLParenLoc,
    BridgedSourceLoc cRParenLoc, BridgedExternKind cKind,
    BridgedStringRef cModuleName, BridgedStringRef cName);

SWIFT_NAME("BridgedImplementsAttr.createParsed(_:atLoc:range:protocolType:"
           "memberName:memberNameLoc:)")
BridgedImplementsAttr BridgedImplementsAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedTypeRepr cProtocolType,
    BridgedDeclNameRef cMemberName, BridgedDeclNameLoc cMemberNameLoc);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedInlineKind {
  BridgedInlineKindNever,
  BridgedInlineKindAlways,
};

SWIFT_NAME("BridgedInlineAttr.createParsed(_:atLoc:range:kind:)")
BridgedInlineAttr BridgedInlineAttr_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cAtLoc,
                                                 BridgedSourceRange cRange,
                                                 BridgedInlineKind cKind);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedMacroSyntax {
  BridgedMacroSyntaxFreestanding,
  BridgedMacroSyntaxAttached,
};

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedMacroIntroducedDeclNameKind {
  BridgedMacroIntroducedDeclNameKindNamed,
  BridgedMacroIntroducedDeclNameKindOverloaded,
  BridgedMacroIntroducedDeclNameKindPrefixed,
  BridgedMacroIntroducedDeclNameKindSuffixed,
  BridgedMacroIntroducedDeclNameKindArbitrary,
};

BRIDGED_INLINE swift::MacroIntroducedDeclNameKind
unbridge(BridgedMacroIntroducedDeclNameKind kind);

struct BridgedMacroIntroducedDeclName {
  BridgedMacroIntroducedDeclNameKind kind;
  BridgedDeclNameRef name;

  BRIDGED_INLINE swift::MacroIntroducedDeclName unbridged() const;
};

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedMacroRole {
#define MACRO_ROLE(Name, Description) BridgedMacroRole##Name,
#include "swift/Basic/MacroRoles.def"
  BridgedMacroRoleNone,
};

BRIDGED_INLINE swift::MacroRole unbridge(BridgedMacroRole cRole);

SWIFT_NAME("BridgedMacroRole.init(from:)")
BridgedMacroRole BridgedMacroRole_fromString(BridgedStringRef str);

SWIFT_NAME("getter:BridgedMacroRole.isAttached(self:)")
BRIDGED_INLINE bool BridgedMacroRole_isAttached(BridgedMacroRole role);

SWIFT_NAME("BridgedMacroRoleAttr.createParsed(_:atLoc:range:syntax:lParenLoc:"
           "role:names:conformances:rParenLoc:)")
BridgedMacroRoleAttr BridgedMacroRoleAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedMacroSyntax cSyntax,
    BridgedSourceLoc cLParenLoc, BridgedMacroRole cRole, BridgedArrayRef cNames,
    BridgedArrayRef cConformances, BridgedSourceLoc cRParenLoc);

SWIFT_NAME("BridgedStorageRestrictionsAttr.createParsed(_:atLoc:range:"
           "initializes:accesses:)")
BridgedStorageRestrictionsAttr BridgedStorageRestrictionsAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedArrayRef cInitializes,
    BridgedArrayRef cAccesses);

SWIFT_NAME(
    "BridgedSwiftNativeObjCRuntimeBaseAttr.createParsed(_:atLoc:range:name:)")
BridgedSwiftNativeObjCRuntimeBaseAttr
BridgedSwiftNativeObjCRuntimeBaseAttr_createParsed(BridgedASTContext cContext,
                                                   BridgedSourceLoc cAtLoc,
                                                   BridgedSourceRange cRange,
                                                   BridgedIdentifier cName);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedNonSendableKind {
  BridgedNonSendableKindSpecific,
  BridgedNonSendableKindAssumed,
};

SWIFT_NAME("BridgedNonSendableAttr.createParsed(_:atLoc:range:kind:)")
BridgedNonSendableAttr BridgedNonSendableAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedNonSendableKind cKind);

SWIFT_NAME("BridgedNonisolatedAttr.createParsed(_:atLoc:range:isUnsafe:)")
BridgedNonisolatedAttr
BridgedNonisolatedAttr_createParsed(BridgedASTContext cContext,
                                    BridgedSourceLoc cAtLoc,
                                    BridgedSourceRange cRange, bool isUnsafe);

SWIFT_NAME("BridgedObjCAttr.createParsedUnnamed(_:atLoc:attrNameLoc:)")
BridgedObjCAttr
BridgedObjCAttr_createParsedUnnamed(BridgedASTContext cContext,
                                    BridgedSourceLoc cAtLoc,
                                    BridgedSourceLoc cAttrNameLoc);

SWIFT_NAME("BridgedObjCAttr.createParsedNullary(_:atLoc:attrNameLoc:lParenLoc:"
           "nameLoc:name:rParenLoc:)")
BridgedObjCAttr BridgedObjCAttr_createParsedNullary(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceLoc cAttrNameLoc, BridgedSourceLoc cLParenLoc,
    BridgedSourceLoc cNameLoc, BridgedIdentifier cName,
    BridgedSourceLoc cRParenLoc);

SWIFT_NAME("BridgedObjCAttr.createParsedSelector(_:atLoc:attrNameLoc:lParenLoc:"
           "nameLocs:names:rParenLoc:)")
BridgedObjCAttr BridgedObjCAttr_createParsedSelector(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceLoc cAttrNameLoc, BridgedSourceLoc cLParenLoc,
    BridgedArrayRef cNameLocs, BridgedArrayRef cNames,
    BridgedSourceLoc cRParenLoc);

SWIFT_NAME("BridgedObjCImplementationAttr.createParsed(_:atLoc:range:name:isEarlyAdopter:)")
BridgedObjCImplementationAttr BridgedObjCImplementationAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedIdentifier cName, bool isEarlyAdopter);

SWIFT_NAME("BridgedObjCRuntimeNameAttr.createParsed(_:atLoc:range:name:)")
BridgedObjCRuntimeNameAttr BridgedObjCRuntimeNameAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedIdentifier cName);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedOptimizationMode {
  BridgedOptimizationModeForSpeed,
  BridgedOptimizationModeForSize,
  BridgedOptimizationModeNoOptimization,
};

SWIFT_NAME("BridgedOptimizeAttr.createParsed(_:atLoc:range:mode:)")
BridgedOptimizeAttr BridgedOptimizeAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedOptimizationMode cMode);

SWIFT_NAME("BridgedPrivateImportAttr.createParsed(_:atLoc:attrNameLoc:"
           "lParenLoc:fileName:rParenLoc:)")
BridgedPrivateImportAttr BridgedPrivateImportAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceLoc cAttrNameLoc, BridgedSourceLoc cLParenLoc,
    BridgedStringRef cFileName, BridgedSourceLoc cRParenLoc);

SWIFT_NAME(
    "BridgedProjectedValuePropertyAttr.createParsed(_:atLoc:range:name:)")
BridgedProjectedValuePropertyAttr
BridgedProjectedValuePropertyAttr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cAtLoc,
                                               BridgedSourceRange cRange,
                                               BridgedIdentifier cName);

SWIFT_NAME("BridgedRawDocCommentAttr.createParsed(_:range:)")
BridgedRawDocCommentAttr
BridgedRawDocCommentAttr_createParsed(BridgedASTContext cContext,
                                      BridgedCharSourceRange cRange);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedReferenceOwnership {
  BridgedReferenceOwnershipWeak,
  BridgedReferenceOwnershipUnowned,
  BridgedReferenceOwnershipUnmanaged,
};

SWIFT_NAME("BridgedReferenceOwnershipAttr.createParsed(_:atLoc:range:kind:)")
BridgedReferenceOwnershipAttr BridgedReferenceOwnershipAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedReferenceOwnership cKind);

SWIFT_NAME("BridgedSectionAttr.createParsed(_:atLoc:range:name:)")
BridgedSectionAttr BridgedSectionAttr_createParsed(BridgedASTContext cContext,
                                                   BridgedSourceLoc cAtLoc,
                                                   BridgedSourceRange cRange,
                                                   BridgedStringRef cName);

SWIFT_NAME("BridgedSemanticsAttr.createParsed(_:atLoc:range:value:)")
BridgedSemanticsAttr BridgedSemanticsAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedStringRef cValue);

SWIFT_NAME("BridgedSetterAccessAttr.createParsed(_:range:accessLevel:)")
BridgedSetterAccessAttr
BridgedSetterAccessAttr_createParsed(BridgedASTContext cContext,
                                     BridgedSourceRange cRange,
                                     BridgedAccessLevel cAccessLevel);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedSpecializationKind : uint8_t {
  BridgedSpecializationKindFull,
  BridgedSpecializationKindPartial,
};

SWIFT_NAME("BridgedSpecializeAttr.createParsed(_:atLoc:range:whereClause:"
           "exported:kind:taretFunction:spiGroups:availableAttrs:)")
BridgedSpecializeAttr BridgedSpecializeAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedNullableTrailingWhereClause cWhereClause,
    bool exported, BridgedSpecializationKind cKind,
    BridgedDeclNameRef cTargetFunction, BridgedArrayRef cSPIGroups,
    BridgedArrayRef cAvailableAttrs);

SWIFT_NAME(
    "BridgedSPIAccessControlAttr.createParsed(_:atLoc:range:spiGroupName:)")
BridgedSPIAccessControlAttr BridgedSPIAccessControlAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedIdentifier cSPIGroupName);

SWIFT_NAME("BridgedSILGenNameAttr.createParsed(_:atLoc:range:name:isRaw:)")
BridgedSILGenNameAttr BridgedSILGenNameAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedStringRef cName, bool isRaw);

SWIFT_NAME(
    "BridgedUnavailableFromAsyncAttr.createParsed(_:atLoc:range:message:)")
BridgedUnavailableFromAsyncAttr BridgedUnavailableFromAsyncAttr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAtLoc,
    BridgedSourceRange cRange, BridgedStringRef cMessage);

//===----------------------------------------------------------------------===//
// MARK: Decls
//===----------------------------------------------------------------------===//

struct BridgedFingerprint;

SWIFT_NAME("BridgedDecl.setAttrs(self:_:)")
void BridgedDecl_setAttrs(BridgedDecl decl, BridgedDeclAttributes attrs);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedStaticSpelling {
  BridgedStaticSpellingNone,
  BridgedStaticSpellingStatic,
  BridgedStaticSpellingClass
};

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedAccessorKind {
#define ACCESSOR(ID) BridgedAccessorKind##ID,
#include "swift/AST/AccessorKinds.def"
};

struct BridgedAccessorRecord {
  BridgedSourceLoc lBraceLoc;
  BridgedArrayRef accessors;
  BridgedSourceLoc rBraceLoc;
};

SWIFT_NAME("BridgedAccessorDecl.createParsed(_:declContext:kind:storage:"
           "declLoc:accessorKeywordLoc:parameterList:asyncSpecifierLoc:"
           "throwsSpecifierLoc:thrownType:)")
BridgedAccessorDecl BridgedAccessorDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedAccessorKind cKind, BridgedAbstractStorageDecl cStorage,
    BridgedSourceLoc cDeclLoc, BridgedSourceLoc cAccessorKeywordLoc,
    BridgedNullableParameterList cParamList, BridgedSourceLoc cAsyncLoc,
    BridgedSourceLoc cThrowsLoc, BridgedNullableTypeRepr cThrownType);

SWIFT_NAME(
    "BridgedPatternBindingDecl.createParsed(_:declContext:bindingKeywordLoc:"
    "entries:attributes:isStatic:isLet:)")
BridgedPatternBindingDecl BridgedPatternBindingDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cBindingKeywordLoc, BridgedArrayRef cBindingEntries, BridgedDeclAttributes cAttrs,
                                                                 bool isStatic, bool isLet);

SWIFT_NAME("BridgedParamDecl.createParsed(_:declContext:specifierLoc:argName:"
           "argNameLoc:paramName:paramNameLoc:type:defaultValue:)")
BridgedParamDecl BridgedParamDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cSpecifierLoc, BridgedIdentifier cArgName,
    BridgedSourceLoc cArgNameLoc, BridgedIdentifier cParamName,
    BridgedSourceLoc cParamNameLoc, BridgedNullableTypeRepr type,
    BridgedNullableExpr defaultValue);

/// The various spellings of ownership modifier that can be used in source.
enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedParamSpecifier {
  BridgedParamSpecifierDefault,
  BridgedParamSpecifierInOut,
  BridgedParamSpecifierBorrowing,
  BridgedParamSpecifierConsuming,
  BridgedParamSpecifierLegacyShared,
  BridgedParamSpecifierLegacyOwned,
  BridgedParamSpecifierImplicitlyCopyableConsuming,
};

BRIDGED_INLINE swift::ParamSpecifier unbridge(BridgedParamSpecifier kind);

SWIFT_NAME("BridgedParamDecl.setSpecifier(self:_:)")
BRIDGED_INLINE void
BridgedParamDecl_setSpecifier(BridgedParamDecl cDecl,
                              BridgedParamSpecifier cSpecifier);

SWIFT_NAME("BridgedConstructorDecl.setParsedBody(self:_:)")
void BridgedConstructorDecl_setParsedBody(BridgedConstructorDecl decl,
                                          BridgedBraceStmt body);

SWIFT_NAME("BridgedFuncDecl.setParsedBody(self:_:)")
void BridgedFuncDecl_setParsedBody(BridgedFuncDecl decl, BridgedBraceStmt body);

SWIFT_NAME("BridgedDestructorDecl.setParsedBody(self:_:)")
void BridgedDestructorDecl_setParsedBody(BridgedDestructorDecl decl,
                                         BridgedBraceStmt body);

SWIFT_NAME("BridgedFuncDecl.createParsed(_:declContext:staticLoc:"
           "staticSpelling:funcKeywordLoc:"
           "name:nameLoc:genericParamList:parameterList:asyncSpecifierLoc:"
           "throwsSpecifierLoc:thrownType:returnType:genericWhereClause:)")
BridgedFuncDecl BridgedFuncDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStaticLoc, BridgedStaticSpelling cStaticSpelling,
    BridgedSourceLoc cFuncKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedParameterList parameterList, BridgedSourceLoc cAsyncLoc,
    BridgedSourceLoc cThrowsLoc, BridgedNullableTypeRepr thrownType,
    BridgedNullableTypeRepr returnType,
    BridgedNullableTrailingWhereClause opaqueGenericWhereClause);

SWIFT_NAME(
    "BridgedConstructorDecl.createParsed(_:declContext:initKeywordLoc:"
    "failabilityMarkLoc:isIUO:genericParamList:parameterList:"
    "asyncSpecifierLoc:throwsSpecifierLoc:thrownType:genericWhereClause:)")
BridgedConstructorDecl BridgedConstructorDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cInitKeywordLoc, BridgedSourceLoc cFailabilityMarkLoc,
    bool isIUO, BridgedNullableGenericParamList genericParams,
    BridgedParameterList parameterList, BridgedSourceLoc cAsyncLoc,
    BridgedSourceLoc cThrowsLoc, BridgedNullableTypeRepr thrownType,
    BridgedNullableTrailingWhereClause genericWhereClause);

SWIFT_NAME(
    "BridgedDestructorDecl.createParsed(_:declContext:deinitKeywordLoc:)")
BridgedDestructorDecl
BridgedDestructorDecl_createParsed(BridgedASTContext cContext,
                                   BridgedDeclContext cDeclContext,
                                   BridgedSourceLoc cDeinitKeywordLoc);

SWIFT_NAME(
    "BridgedTypeAliasDecl.createParsed(_:declContext:typealiasKeywordLoc:name:"
    "nameLoc:genericParamList:equalLoc:underlyingType:genericWhereClause:)")
BridgedTypeAliasDecl BridgedTypeAliasDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cAliasKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedSourceLoc cEqualLoc, BridgedTypeRepr underlyingType,
    BridgedNullableTrailingWhereClause genericWhereClause);

SWIFT_NAME("BridgedExtensionDecl.setParsedMembers(self:_:fingerprint:)")
void BridgedExtensionDecl_setParsedMembers(BridgedExtensionDecl decl,
                                           BridgedArrayRef members,
                                           BridgedFingerprint fingerprint);

SWIFT_NAME(
    "BridgedEnumDecl.createParsed(_:declContext:enumKeywordLoc:name:nameLoc:"
    "genericParamList:inheritedTypes:genericWhereClause:braceRange:)")
BridgedNominalTypeDecl BridgedEnumDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cEnumKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange);

SWIFT_NAME(
    "BridgedEnumCaseDecl.createParsed(declContext:caseKeywordLoc:elements:)")
BridgedEnumCaseDecl
BridgedEnumCaseDecl_createParsed(BridgedDeclContext cDeclContext,
                                 BridgedSourceLoc cCaseKeywordLoc,
                                 BridgedArrayRef cElements);

SWIFT_NAME("BridgedEnumElementDecl.createParsed(_:declContext:name:nameLoc:"
           "parameterList:equalsLoc:rawValue:)")
BridgedEnumElementDecl BridgedEnumElementDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedIdentifier cName, BridgedSourceLoc cNameLoc,
    BridgedNullableParameterList parameterList, BridgedSourceLoc cEqualsLoc,
    BridgedNullableExpr opaqueRawValue);

SWIFT_NAME("BridgedStructDecl.createParsed(_:declContext:structKeywordLoc:name:"
           "nameLoc:genericParamList:inheritedTypes:genericWhereClause:"
           "braceRange:)")
BridgedNominalTypeDecl BridgedStructDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStructKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange);

SWIFT_NAME(
    "BridgedClassDecl.createParsed(_:declContext:classKeywordLoc:name:nameLoc:"
    "genericParamList:inheritedTypes:genericWhereClause:braceRange:isActor:)")
BridgedNominalTypeDecl BridgedClassDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cClassKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList genericParamList,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange, bool isActor);

SWIFT_NAME(
    "BridgedProtocolDecl.createParsed(_:declContext:protocolKeywordLoc:name:"
    "nameLoc:primaryAssociatedTypeNames:inheritedTypes:"
    "genericWhereClause:braceRange:)")
BridgedNominalTypeDecl BridgedProtocolDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cProtocolKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedArrayRef cPrimaryAssociatedTypeNames,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange);

SWIFT_NAME("BridgedAssociatedTypeDecl.createParsed(_:declContext:"
           "associatedtypeKeywordLoc:name:nameLoc:inheritedTypes:defaultType:"
           "genericWhereClause:)")
BridgedAssociatedTypeDecl BridgedAssociatedTypeDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cAssociatedtypeKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedArrayRef cInheritedTypes,
    BridgedNullableTypeRepr opaqueDefaultType,
    BridgedNullableTrailingWhereClause genericWhereClause);

SWIFT_NAME(
    "BridgedMacroDecl.createParsed(_:declContext:macroKeywordLoc:name:nameLoc:"
    "genericParamList:paramList:arrowLoc:resultType:definition:)")
BridgedMacroDecl BridgedMacroDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cMacroLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableGenericParamList cGenericParams,
    BridgedParameterList cParams, BridgedSourceLoc cArrowLoc,
    BridgedNullableTypeRepr cResultType, BridgedNullableExpr cDefinition);

SWIFT_NAME("BridgedMacroExpansionDecl.createParsed(_:poundLoc:macroNameRef:"
           "macroNameLoc:leftAngleLoc:genericArgs:rightAngleLoc:args:)")
BridgedMacroExpansionDecl BridgedMacroExpansionDecl_createParsed(
    BridgedDeclContext cDeclContext, BridgedSourceLoc cPoundLoc,
    BridgedDeclNameRef cMacroNameRef, BridgedDeclNameLoc cMacroNameLoc,
    BridgedSourceLoc cLeftAngleLoc, BridgedArrayRef cGenericArgs,
    BridgedSourceLoc cRightAngleLoc, BridgedNullableArgumentList cArgList);

SWIFT_NAME(
    "BridgedExtensionDecl.createParsed(_:declContext:extensionKeywordLoc:"
    "extendedType:inheritedTypes:genericWhereClause:braceRange:)")
BridgedExtensionDecl BridgedExtensionDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cExtensionKeywordLoc, BridgedTypeRepr opaqueExtendedType,
    BridgedArrayRef cInheritedTypes,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceRange cBraceRange);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedOperatorFixity {
  BridgedOperatorFixityInfix,
  BridgedOperatorFixityPrefix,
  BridgedOperatorFixityPostfix,
};

SWIFT_NAME("BridgedOperatorDecl.createParsed(_:declContext:fixity:"
           "operatorKeywordLoc:name:nameLoc:colonLoc:precedenceGroupName:"
           "precedenceGroupLoc:)")
BridgedOperatorDecl BridgedOperatorDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedOperatorFixity cFixity, BridgedSourceLoc cOperatorKeywordLoc,
    BridgedIdentifier cName, BridgedSourceLoc cNameLoc,
    BridgedSourceLoc cColonLoc, BridgedIdentifier cPrecedenceGroupName,
    BridgedSourceLoc cPrecedenceGroupLoc);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedAssociativity {
  BridgedAssociativityNone,
  BridgedAssociativityLeft,
  BridgedAssociativityRight,
};

SWIFT_NAME("BridgedPrecedenceGroupDecl.createParsed(declContext:"
           "precedencegroupKeywordLoc:name:nameLoc:leftBraceLoc:"
           "associativityLabelLoc:associativityValueLoc:associativity:"
           "assignmentLabelLoc:assignmentValueLoc:isAssignment:"
           "higherThanKeywordLoc:higherThanNames:lowerThanKeywordLoc:"
           "lowerThanNames:rightBraceLoc:)")
BridgedPrecedenceGroupDecl BridgedPrecedenceGroupDecl_createParsed(
    BridgedDeclContext cDeclContext,
    BridgedSourceLoc cPrecedencegroupKeywordLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedSourceLoc cLeftBraceLoc,
    BridgedSourceLoc cAssociativityKeywordLoc,
    BridgedSourceLoc cAssociativityValueLoc,
    BridgedAssociativity cAssociativity, BridgedSourceLoc cAssignmentKeywordLoc,
    BridgedSourceLoc cAssignmentValueLoc, bool isAssignment,
    BridgedSourceLoc cHigherThanKeywordLoc, BridgedArrayRef cHigherThanNames,
    BridgedSourceLoc cLowerThanKeywordLoc, BridgedArrayRef cLowerThanNames,
    BridgedSourceLoc cRightBraceLoc);

enum ENUM_EXTENSIBILITY_ATTR(open) BridgedImportKind {
  BridgedImportKindModule,
  BridgedImportKindType,
  BridgedImportKindStruct,
  BridgedImportKindClass,
  BridgedImportKindEnum,
  BridgedImportKindProtocol,
  BridgedImportKindVar,
  BridgedImportKindFunc,
};

SWIFT_NAME("BridgedImportDecl.createParsed(_:declContext:importKeywordLoc:"
           "importKind:importKindLoc:path:)")
BridgedImportDecl BridgedImportDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cImportKeywordLoc, BridgedImportKind cImportKind,
    BridgedSourceLoc cImportKindLoc, BridgedArrayRef cImportPathElements);

SWIFT_NAME("BridgedSubscriptDecl.createParsed(_:declContext:staticLoc:"
           "staticSpelling:subscriptKeywordLoc:genericParamList:parameterList:"
           "arrowLoc:returnType:)")
BridgedSubscriptDecl BridgedSubscriptDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStaticLoc, BridgedStaticSpelling cStaticSpelling,
    BridgedSourceLoc cSubscriptKeywordLoc,
    BridgedNullableGenericParamList cGenericParamList,
    BridgedParameterList cParamList, BridgedSourceLoc cArrowLoc,
    BridgedTypeRepr returnType);

SWIFT_NAME(
    "BridgedTopLevelCodeDecl.createParsed(_:declContext:startLoc:stmt:endLoc:)")
BridgedTopLevelCodeDecl BridgedTopLevelCodeDecl_createStmt(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStartLoc, BridgedStmt statement,
    BridgedSourceLoc cEndLoc);

SWIFT_NAME(
    "BridgedTopLevelCodeDecl.createParsed(_:declContext:startLoc:expr:endLoc:)")
BridgedTopLevelCodeDecl BridgedTopLevelCodeDecl_createExpr(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cStartLoc, BridgedExpr expression,
    BridgedSourceLoc cEndLoc);

SWIFT_NAME("BridgedTopLevelCodeDecl.dump(self:)")
void BridgedTopLevelCodeDecl_dump(BridgedTopLevelCodeDecl decl);

SWIFT_NAME("BridgedDecl.dump(self:)")
void BridgedDecl_dump(BridgedDecl decl);

//===----------------------------------------------------------------------===//
// MARK: AbstractStorageDecl
//===----------------------------------------------------------------------===//

SWIFT_NAME("BridgedAbstractStorageDecl.setAccessors(self:_:)")
void BridgedAbstractStorageDecl_setAccessors(
    BridgedAbstractStorageDecl cStorage, BridgedAccessorRecord accessors);

//===----------------------------------------------------------------------===//
// MARK: AccessorDecl
//===----------------------------------------------------------------------===//

SWIFT_NAME("BridgedAccessorDecl.setParsedBody(self:_:)")
void BridgedAccessorDecl_setParsedBody(BridgedAccessorDecl decl,
                                       BridgedBraceStmt body);

//===----------------------------------------------------------------------===//
// MARK: NominalTypeDecl
//===----------------------------------------------------------------------===//

SWIFT_NAME("BridgedNominalTypeDecl.getName(self:)")
BRIDGED_INLINE
BridgedStringRef BridgedNominalTypeDecl_getName(BridgedNominalTypeDecl decl);

SWIFT_NAME("BridgedNominalTypeDecl.isStructWithUnreferenceableStorage(self:)")
bool BridgedNominalTypeDecl_isStructWithUnreferenceableStorage(
    BridgedNominalTypeDecl decl);

SWIFT_NAME("BridgedNominalTypeDecl.isGlobalActor(self:)")
BRIDGED_INLINE
bool BridgedNominalTypeDecl_isGlobalActor(BridgedNominalTypeDecl decl);

SWIFT_NAME("BridgedNominalTypeDecl.hasValueDeinit(self:)")
BRIDGED_INLINE
bool BridgedNominalTypeDecl_hasValueDeinit(BridgedNominalTypeDecl decl);

SWIFT_NAME("BridgedNominalTypeDecl.isClass(self:)")
BRIDGED_INLINE
bool BridgedNominalTypeDecl_isClass(BridgedNominalTypeDecl decl);

SWIFT_NAME("BridgedNominalTypeDecl.isGenericAtAnyLevel(self:)")
BRIDGED_INLINE
bool BridgedNominalTypeDecl_isGenericAtAnyLevel(BridgedNominalTypeDecl decl);

SWIFT_NAME("BridgedNominalTypeDecl.setParsedMembers(self:_:fingerprint:)")
void BridgedNominalTypeDecl_setParsedMembers(BridgedNominalTypeDecl decl,
                                             BridgedArrayRef members,
                                             BridgedFingerprint fingerprint);

SWIFT_NAME("BridgedNominalTypeDecl.getSourceLocation(self:)")
BRIDGED_INLINE BridgedSourceLoc BridgedNominalTypeDecl_getSourceLocation(BridgedNominalTypeDecl decl);

//===----------------------------------------------------------------------===//
// MARK: SubscriptDecl
//===----------------------------------------------------------------------===//

SWIFT_NAME("getter:BridgedSubscriptDecl.asAbstractStorageDecl(self:)")
BRIDGED_INLINE
BridgedAbstractStorageDecl
BridgedSubscriptDecl_asAbstractStorageDecl(BridgedSubscriptDecl decl);

//===----------------------------------------------------------------------===//
// MARK: VarDecl
//===----------------------------------------------------------------------===//

SWIFT_NAME("BridgedVarDecl.createImplicitStringInterpolationVar(_:)")
BridgedVarDecl BridgedVarDec_createImplicitStringInterpolationVar(
    BridgedDeclContext cDeclContext);

SWIFT_NAME("getter:BridgedVarDecl.asAbstractStorageDecl(self:)")
BRIDGED_INLINE
BridgedAbstractStorageDecl
BridgedVarDecl_asAbstractStorageDecl(BridgedVarDecl decl);

//===----------------------------------------------------------------------===//
// MARK: Exprs
//===----------------------------------------------------------------------===//

struct BridgedCallArgument {
  BridgedSourceLoc labelLoc;
  BridgedIdentifier label;
  BridgedExpr argExpr;

  BRIDGED_INLINE swift::Argument unbridged() const;
};

SWIFT_NAME("BridgedArgumentList.createImplicitUnlabeled(_:exprs:)")
BridgedArgumentList
BridgedArgumentList_createImplicitUnlabeled(BridgedASTContext cContext,
                                            BridgedArrayRef cExprs);

SWIFT_NAME("BridgedArgumentList.createParsed(_:lParenLoc:args:rParenLoc:"
           "firstTrailingClosureIndex:)")
BridgedArgumentList BridgedArgumentList_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLParenLoc,
    BridgedArrayRef cArgs, BridgedSourceLoc cRParenLoc,
    size_t cFirstTrailingClosureIndex);

SWIFT_NAME("BridgedArrayExpr.createParsed(_:lSquareLoc:elements:commaLocs:"
           "rSquareLoc:)")
BridgedArrayExpr BridgedArrayExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cLLoc,
                                               BridgedArrayRef elements,
                                               BridgedArrayRef commas,
                                               BridgedSourceLoc cRLoc);

SWIFT_NAME(
    "BridgedArrowExpr.createParsed(_:asyncLoc:throwsLoc:thrownType:arrowLoc:)")
BridgedArrowExpr BridgedArrowExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cAsyncLoc,
                                               BridgedSourceLoc cThrowsLoc,
                                               BridgedNullableExpr cThrownType,
                                               BridgedSourceLoc cArrowLoc);

SWIFT_NAME("BridgedAssignExpr.createParsed(_:equalsLoc:)")
BridgedAssignExpr BridgedAssignExpr_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cEqualsLoc);

SWIFT_NAME("BridgedAwaitExpr.createParsed(_:awaitLoc:subExpr:)")
BridgedAwaitExpr BridgedAwaitExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cAwaitLoc,
                                               BridgedExpr cSubExpr);

SWIFT_NAME("BridgedBindOptionalExpr.createParsed(_:subExpr:questionLoc:)")
BridgedBindOptionalExpr
BridgedBindOptionalExpr_createParsed(BridgedASTContext cContext,
                                     BridgedExpr cSubExpr,
                                     BridgedSourceLoc cQuestionLoc);

SWIFT_NAME("BridgedBooleanLiteralExpr.createParsed(_:value:loc:)")
BridgedBooleanLiteralExpr
BridgedBooleanLiteralExpr_createParsed(BridgedASTContext cContext, bool value,
                                       BridgedSourceLoc cTokenLoc);

SWIFT_NAME("BridgedBorrowExpr.createParsed(_:borrowLoc:subExpr:)")
BridgedBorrowExpr BridgedBorrowExpr_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cBorrowLoc,
                                                 BridgedExpr cSubExpr);

SWIFT_NAME("BridgedCallExpr.createParsed(_:fn:args:)")
BridgedCallExpr BridgedCallExpr_createParsed(BridgedASTContext cContext,
                                             BridgedExpr fn,
                                             BridgedArgumentList args);

SWIFT_NAME("BridgedClosureExpr.createParsed(_:declContext:attributes:"
           "bracketRange:capturedSelfDecl:parameterList:asyncLoc:throwsLoc:"
           "thrownType:arrowLoc:explicitResultType:inLoc:)")
BridgedClosureExpr BridgedClosureExpr_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedDeclAttributes cAttributes, BridgedSourceRange cBracketRange,
    BridgedNullableVarDecl cCapturedSelfDecl,
    BridgedNullableParameterList cParameterList, BridgedSourceLoc cAsyncLoc,
    BridgedSourceLoc cThrowsLoc, BridgedNullableTypeRepr cThrownType,
    BridgedSourceLoc cArrowLoc, BridgedNullableTypeRepr cExplicitResultType,
    BridgedSourceLoc cInLoc);

SWIFT_NAME("BridgedClosureExpr.setParameterList(self:_:)")
void BridgedClosureExpr_setParameterList(BridgedClosureExpr cClosure,
                                         BridgedParameterList cParams);

SWIFT_NAME("BridgedClosureExpr.setHasAnonymousClosureVars(self:)")
void BridgedClosureExpr_setHasAnonymousClosureVars(BridgedClosureExpr cClosure);

SWIFT_NAME("BridgedClosureExpr.setBody(self:_:)")
void BridgedClosureExpr_setBody(BridgedClosureExpr cClosure,
                                BridgedBraceStmt cBody);

SWIFT_NAME("BridgedCoerceExpr.createParsed(_:asLoc:type:)")
BridgedCoerceExpr BridgedCoerceExpr_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cAsLoc,
                                                 BridgedTypeRepr cType);

SWIFT_NAME(
    "BridgedConditionalCheckedCastExpr.createParsed(_:asLoc:questionLoc:type:)")
BridgedConditionalCheckedCastExpr
BridgedConditionalCheckedCastExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cAsLoc,
                                               BridgedSourceLoc cQuestionLoc,
                                               BridgedTypeRepr cType);

SWIFT_NAME("BridgedConsumeExpr.createParsed(_:consumeLoc:subExpr:)")
BridgedConsumeExpr BridgedConsumeExpr_createParsed(BridgedASTContext cContext,
                                                   BridgedSourceLoc cConsumeLoc,
                                                   BridgedExpr cSubExpr);

SWIFT_NAME("BridgedCopyExpr.createParsed(_:copyLoc:subExpr:)")
BridgedCopyExpr BridgedCopyExpr_createParsed(BridgedASTContext cContext,
                                             BridgedSourceLoc cCopyLoc,
                                             BridgedExpr cSubExpr);

SWIFT_NAME("BridgedDeclRefExpr.create(_:decl:loc:isImplicit:)")
BridgedDeclRefExpr BridgedDeclRefExpr_create(BridgedASTContext cContext,
                                             BridgedDecl cDecl,
                                             BridgedDeclNameLoc cLoc,
                                             bool IsImplicit);

SWIFT_NAME("BridgedDictionaryExpr.createParsed(_:lBracketLoc:elements:"
           "colonLocs:rBracketLoc:)")
BridgedDictionaryExpr BridgedDictionaryExpr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLBracketLoc,
    BridgedArrayRef cElements, BridgedArrayRef cCommaLocs,
    BridgedSourceLoc cRBracketLoc);

SWIFT_NAME("BridgedDiscardAssignmentExpr.createParsed(_:loc:)")
BridgedDiscardAssignmentExpr
BridgedDiscardAssignmentExpr_createParsed(BridgedASTContext cContext,
                                          BridgedSourceLoc cLoc);

SWIFT_NAME("BridgedDotSelfExpr.createParsed(_:subExpr:dotLoc:selfLoc:)")
BridgedDotSelfExpr BridgedDotSelfExpr_createParsed(BridgedASTContext cContext,
                                                   BridgedExpr cSubExpr,
                                                   BridgedSourceLoc cDotLoc,
                                                   BridgedSourceLoc cSelfLoc);

SWIFT_NAME("BridgedEditorPlaceholderExpr.createParsed(_:placeholder:loc:"
           "placeholderType:expansionType:)")
BridgedEditorPlaceholderExpr BridgedEditorPlaceholderExpr_createParsed(
    BridgedASTContext cContext, BridgedIdentifier cPlaceholderId,
    BridgedSourceLoc cLoc, BridgedNullableTypeRepr cPlaceholderTyR,
    BridgedNullableTypeRepr cExpansionTyR);

SWIFT_NAME("BridgedErrorExpr.create(_:loc:)")
BridgedErrorExpr BridgedErrorExpr_create(BridgedASTContext cContext,
                                         BridgedSourceRange cRange);

SWIFT_NAME("BridgedFloatLiteralExpr.createParsed(_:value:loc:)")
BridgedFloatLiteralExpr
BridgedFloatLiteralExpr_createParsed(BridgedASTContext cContext,
                                     BridgedStringRef cStr,
                                     BridgedSourceLoc cTokenLoc);

SWIFT_NAME("BridgedForceTryExpr.createParsed(_:tryLoc:subExpr:exclaimLoc:)")
BridgedForceTryExpr
BridgedForceTryExpr_createParsed(BridgedASTContext cContext,
                                 BridgedSourceLoc cTryLoc, BridgedExpr cSubExpr,
                                 BridgedSourceLoc cExclaimLoc);

SWIFT_NAME("BridgedForceValueExpr.createParsed(_:subExpr:exclaimLoc:)")
BridgedForceValueExpr
BridgedForceValueExpr_createParsed(BridgedASTContext cContext,
                                   BridgedExpr cSubExpr,
                                   BridgedSourceLoc cExclaimLoc);

SWIFT_NAME(
    "BridgedForcedCheckedCastExpr.createParsed(_:asLoc:exclaimLoc:type:)")
BridgedForcedCheckedCastExpr BridgedForcedCheckedCastExpr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cAsLoc,
    BridgedSourceLoc cExclaimLoc, BridgedTypeRepr cType);

SWIFT_NAME("BridgedUnresolvedSpecializeExpr.createParsed(_:subExpr:lAngleLoc:"
           "arguments:rAngleLoc:)")
BridgedUnresolvedSpecializeExpr BridgedUnresolvedSpecializeExpr_createParsed(
    BridgedASTContext cContext, BridgedExpr cSubExpr,
    BridgedSourceLoc cLAngleLoc, BridgedArrayRef cArguments,
    BridgedSourceLoc cRAngleLoc);

SWIFT_NAME("BridgedInOutExpr.createParsed(_:loc:subExpr:)")
BridgedInOutExpr BridgedInOutExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cLoc,
                                               BridgedExpr cSubExpr);

SWIFT_NAME("BridgedIntegerLiteralExpr.createParsed(_:value:loc:)")
BridgedIntegerLiteralExpr
BridgedIntegerLiteralExpr_createParsed(BridgedASTContext cContext,
                                       BridgedStringRef cStr,
                                       BridgedSourceLoc cTokenLoc);

SWIFT_NAME("BridgedInterpolatedStringLiteralExpr.createParsed(_:loc:"
           "literalCapacity:interpolationCount:appendingExpr:)")
BridgedInterpolatedStringLiteralExpr
BridgedInterpolatedStringLiteralExpr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLoc, size_t literalCapacity,
    size_t interpolationCount, BridgedTapExpr cAppendingExpr);

SWIFT_NAME("BridgedIsExpr.createParsed(_:isLoc:type:)")
BridgedIsExpr BridgedIsExpr_createParsed(BridgedASTContext cContext,
                                         BridgedSourceLoc cIsLoc,
                                         BridgedTypeRepr cType);

SWIFT_NAME("BridgedKeyPathDotExpr.createParsed(_:loc:)")
BridgedKeyPathDotExpr
BridgedKeyPathDotExpr_createParsed(BridgedASTContext cContext,
                                   BridgedSourceLoc cLoc);

SWIFT_NAME("BridgedKeyPathExpr.createParsed(_:backslashLoc:parsedRoot:"
           "parsedPath:hasLeadingDot:)")
BridgedKeyPathExpr BridgedKeyPathExpr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cBackslashLoc,
    BridgedNullableExpr cParsedRoot, BridgedNullableExpr cParsedPath,
    bool hasLeadingDot);

SWIFT_NAME("BridgedMacroExpansionExpr.createParsed(_:poundLoc:macroNameRef:"
           "macroNameLoc:leftAngleLoc:genericArgs:rightAngleLoc:args:)")
BridgedMacroExpansionExpr BridgedMacroExpansionExpr_createParsed(
    BridgedDeclContext cDeclContext, BridgedSourceLoc cPoundLoc,
    BridgedDeclNameRef cMacroNameRef, BridgedDeclNameLoc cMacroNameLoc,
    BridgedSourceLoc cLeftAngleLoc, BridgedArrayRef cGenericArgs,
    BridgedSourceLoc cRightAngleLoc, BridgedNullableArgumentList cArgList);

SWIFT_NAME("BridgedNilLiteralExpr.createParsed(_:nilKeywordLoc:)")
BridgedNilLiteralExpr
BridgedNilLiteralExpr_createParsed(BridgedASTContext cContext,
                                   BridgedSourceLoc cNilKeywordLoc);

SWIFT_NAME("BridgedOptionalTryExpr.createParsed(_:tryLoc:subExpr:questionLoc:)")
BridgedOptionalTryExpr BridgedOptionalTryExpr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cTryLoc, BridgedExpr cSubExpr,
    BridgedSourceLoc cQuestionLoc);

SWIFT_NAME("BridgedPackElementExpr.createParsed(_:eachLoc:packRefExpr:)")
BridgedPackElementExpr
BridgedPackElementExpr_createParsed(BridgedASTContext cContext,
                                    BridgedSourceLoc cEachLoc,
                                    BridgedExpr cPackRefExpr);

SWIFT_NAME("BridgedPackExpansionExpr.createParsed(_:repeatLoc:patternExpr:)")
BridgedPackExpansionExpr
BridgedPackExpansionExpr_createParsed(BridgedASTContext cContext,
                                      BridgedSourceLoc cRepeatLoc,
                                      BridgedExpr cPatternExpr);

SWIFT_NAME("BridgedParenExpr.createParsed(_:leftParenLoc:expr:rightParenLoc:)")
BridgedParenExpr BridgedParenExpr_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cLParen,
                                               BridgedExpr cExpr,
                                               BridgedSourceLoc cRParen);

SWIFT_NAME("BridgedPostfixUnaryExpr.createParsed(_:operator:operand:)")
BridgedPostfixUnaryExpr
BridgedPostfixUnaryExpr_createParsed(BridgedASTContext cContext,
                                     BridgedExpr oper, BridgedExpr operand);

SWIFT_NAME("BridgedPrefixUnaryExpr.createParsed(_:operator:operand:)")
BridgedPrefixUnaryExpr
BridgedPrefixUnaryExpr_createParsed(BridgedASTContext cContext,
                                    BridgedExpr oper, BridgedExpr operand);

class BridgedRegexLiteralPatternFeatureKind final {
  unsigned RawValue;

public:
  BRIDGED_INLINE
  SWIFT_NAME("init(rawValue:)")
  BridgedRegexLiteralPatternFeatureKind(SwiftInt rawValue);

  using UnbridgedTy = swift::RegexLiteralPatternFeatureKind;

  BRIDGED_INLINE
  BridgedRegexLiteralPatternFeatureKind(UnbridgedTy kind);

  BRIDGED_INLINE
  UnbridgedTy unbridged() const;
};

class BridgedRegexLiteralPatternFeature final {
  BridgedCharSourceRange Range;
  BridgedRegexLiteralPatternFeatureKind Kind;

public:
  SWIFT_NAME("init(kind:at:)")
  BridgedRegexLiteralPatternFeature(BridgedRegexLiteralPatternFeatureKind kind,
                                    BridgedCharSourceRange range)
      : Range(range), Kind(kind) {}

  using UnbridgedTy = swift::RegexLiteralPatternFeature;

  BRIDGED_INLINE
  BridgedRegexLiteralPatternFeature(UnbridgedTy feature);

  BRIDGED_INLINE
  UnbridgedTy unbridged() const;
};

class BridgedRegexLiteralPatternFeatures final {
  BridgedRegexLiteralPatternFeature *_Nullable Data;
  SwiftInt Count;

public:
  BridgedRegexLiteralPatternFeatures() : Data(nullptr), Count(0) {}

  SWIFT_NAME("init(baseAddress:count:)")
  BridgedRegexLiteralPatternFeatures(
      BridgedRegexLiteralPatternFeature *_Nullable data, SwiftInt count)
      : Data(data), Count(count) {}

  using UnbridgedTy = llvm::ArrayRef<BridgedRegexLiteralPatternFeature>;

  BRIDGED_INLINE
  UnbridgedTy unbridged() const;

  SWIFT_IMPORT_UNSAFE
  BridgedRegexLiteralPatternFeature *_Nullable getData() const {
    return Data;
  }
  SwiftInt getCount() const {
    return Count;
  }
};

SWIFT_NAME("BridgedRegexLiteralExpr.createParsed(_:loc:regexText:)")
BridgedRegexLiteralExpr
BridgedRegexLiteralExpr_createParsed(BridgedASTContext cContext,
                                     BridgedSourceLoc cLoc,
                                     BridgedStringRef cRegexText);

SWIFT_NAME("BridgedSequenceExpr.createParsed(_:exprs:)")
BridgedSequenceExpr BridgedSequenceExpr_createParsed(BridgedASTContext cContext,
                                                     BridgedArrayRef exprs);

SWIFT_NAME("BridgedSingleValueStmtExpr.createWithWrappedBranches(_:stmt:"
           "declContext:mustBeExpr:)")
BridgedSingleValueStmtExpr BridgedSingleValueStmtExpr_createWithWrappedBranches(
    BridgedASTContext cContext, BridgedStmt S, BridgedDeclContext cDeclContext,
    bool mustBeExpr);

SWIFT_NAME("BridgedStringLiteralExpr.createParsed(_:value:loc:)")
BridgedStringLiteralExpr
BridgedStringLiteralExpr_createParsed(BridgedASTContext cContext,
                                      BridgedStringRef cStr,
                                      BridgedSourceLoc cTokenLoc);

SWIFT_NAME("BridgedSuperRefExpr.createParsed(_:superLoc:)")
BridgedSuperRefExpr
BridgedSuperRefExpr_createParsed(BridgedASTContext cContext,
                                 BridgedSourceLoc cSuperLoc);

SWIFT_NAME("BridgedSubscriptExpr.createParsed(_:baseExpr:args:)")
BridgedSubscriptExpr
BridgedSubscriptExpr_createParsed(BridgedASTContext cContext,
                                  BridgedExpr cBaseExpr,
                                  BridgedArgumentList cArgs);

SWIFT_NAME("BridgedTapExpr.create(_:body:)")
BridgedTapExpr BridgedTapExpr_create(BridgedASTContext cContext,
                                     BridgedBraceStmt cBody);

SWIFT_NAME("BridgedTernaryExpr.createParsed(_:questionLoc:thenExpr:colonLoc:)")
BridgedTernaryExpr BridgedTernaryExpr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cQuestionLoc,
    BridgedExpr cThenExpr, BridgedSourceLoc cColonLoc);

SWIFT_NAME("BridgedTryExpr.createParsed(_:tryLoc:subExpr:)")
BridgedTryExpr BridgedTryExpr_createParsed(BridgedASTContext cContext,
                                           BridgedSourceLoc cTryLoc,
                                           BridgedExpr cSubExpr);

SWIFT_NAME("BridgedTupleExpr.createParsed(_:leftParenLoc:exprs:labels:"
           "labelLocs:rightParenLoc:)")
BridgedTupleExpr BridgedTupleExpr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLParen, BridgedArrayRef subs,
    BridgedArrayRef names, BridgedArrayRef cNameLocs, BridgedSourceLoc cRParen);

SWIFT_NAME("BridgedTupleExpr.createParsedDictionaryElement(_:key:value:)")
BridgedTupleExpr BridgedTupleExpr_createParsedDictionaryElement(
    BridgedASTContext cContext, BridgedExpr cKeyExpr, BridgedExpr cValueExpr);

SWIFT_NAME("BridgedTypeExpr.createParsed(_:type:)")
BridgedTypeExpr BridgedTypeExpr_createParsed(BridgedASTContext cContext,
                                             BridgedTypeRepr cType);

enum ENUM_EXTENSIBILITY_ATTR(open) BridgedDeclRefKind : size_t {
  BridgedDeclRefKindOrdinary,
  BridgedDeclRefKindBinaryOperator,
  BridgedDeclRefKindPostfixOperator,
  BridgedDeclRefKindPrefixOperator,
};

SWIFT_NAME("BridgedUnresolvedDeclRefExpr.createParsed(_:name:kind:loc:)")
BridgedUnresolvedDeclRefExpr BridgedUnresolvedDeclRefExpr_createParsed(
    BridgedASTContext cContext, BridgedDeclNameRef cName,
    BridgedDeclRefKind cKind, BridgedDeclNameLoc cLoc);

SWIFT_NAME("BridgedUnresolvedDotExpr.createParsed(_:base:dotLoc:name:nameLoc:)")
BridgedUnresolvedDotExpr BridgedUnresolvedDotExpr_createParsed(
    BridgedASTContext cContext, BridgedExpr base, BridgedSourceLoc cDotLoc,
    BridgedDeclNameRef cName, BridgedDeclNameLoc cNameLoc);

SWIFT_NAME("BridgedUnresolvedMemberExpr.createParsed(_:dotLoc:name:nameLoc:)")
BridgedUnresolvedMemberExpr BridgedUnresolvedMemberExpr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cDotLoc,
    BridgedDeclNameRef cName, BridgedDeclNameLoc cNameLoc);

SWIFT_NAME("BridgedUnresolvedPatternExpr.createParsed(_:pattern:)")
BridgedUnresolvedPatternExpr
BridgedUnresolvedPatternExpr_createParsed(BridgedASTContext cContext,
                                          BridgedPattern cPattern);

SWIFT_NAME("BridgedExpr.setImplicit(self:)")
void BridgedExpr_setImplicit(BridgedExpr cExpr);

SWIFT_NAME("BridgedExpr.dump(self:)")
void BridgedExpr_dump(BridgedExpr expr);

//===----------------------------------------------------------------------===//
// MARK: Stmts
//===----------------------------------------------------------------------===//

struct BridgedLabeledStmtInfo {
  SWIFT_NAME("name")
  BridgedIdentifier Name;
  SWIFT_NAME("loc")
  BridgedSourceLoc Loc;

  BRIDGED_INLINE swift::LabeledStmtInfo unbridged() const;
};

class BridgedStmtConditionElement {
  void *_Nonnull Raw;

public:
  BRIDGED_INLINE BridgedStmtConditionElement(swift::StmtConditionElement elem);

  BRIDGED_INLINE swift::StmtConditionElement unbridged() const;
};

SWIFT_NAME("BridgedStmtConditionElement.createBoolean(expr:)")
BridgedStmtConditionElement
BridgedStmtConditionElement_createBoolean(BridgedExpr expr);

SWIFT_NAME("BridgedStmtConditionElement.createPatternBinding(_:introducerLoc:"
           "pattern:initializer:)")
BridgedStmtConditionElement BridgedStmtConditionElement_createPatternBinding(
    BridgedASTContext cContext, BridgedSourceLoc cIntroducerLoc,
    BridgedPattern cPattern, BridgedExpr cInitializer);

struct BridgedCaseLabelItemInfo {
  SWIFT_NAME("isDefault")
  bool IsDefault;
  SWIFT_NAME("pattern")
  BridgedPattern ThePattern;
  SWIFT_NAME("whereLoc")
  BridgedSourceLoc WhereLoc;
  SWIFT_NAME("guardExpr")
  BridgedNullableExpr GuardExpr;
};

SWIFT_NAME("BridgedBraceStmt.createParsed(_:lBraceLoc:elements:rBraceLoc:)")
BridgedBraceStmt BridgedBraceStmt_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cLBLoc,
                                               BridgedArrayRef elements,
                                               BridgedSourceLoc cRBLoc);

SWIFT_NAME("BridgedBreakStmt.createParsed(_:loc:targetName:targetLoc:)")
BridgedBreakStmt BridgedBreakStmt_createParsed(BridgedDeclContext cDeclContext,
                                               BridgedSourceLoc cLoc,
                                               BridgedIdentifier cTargetName,
                                               BridgedSourceLoc cTargetLoc);

SWIFT_NAME("BridgedCaseStmt.createParsedSwitchCase(_:introducerLoc:"
           "caseLabelItems:unknownAttrLoc:terminatorLoc:body:)")
BridgedCaseStmt BridgedCaseStmt_createParsedSwitchCase(
    BridgedASTContext cContext, BridgedSourceLoc cIntroducerLoc,
    BridgedArrayRef cCaseLabelItems, BridgedSourceLoc cUnknownAttrLoc,
    BridgedSourceLoc cTerminatorLoc, BridgedBraceStmt cBody);

SWIFT_NAME(
    "BridgedCaseStmt.createParsedDoCatch(_:catchLoc:caseLabelItems:body:)")
BridgedCaseStmt BridgedCaseStmt_createParsedDoCatch(
    BridgedASTContext cContext, BridgedSourceLoc cCatchLoc,
    BridgedArrayRef cCaseLabelItems, BridgedBraceStmt cBody);

SWIFT_NAME("BridgedContinueStmt.createParsed(_:loc:targetName:targetLoc:)")
BridgedContinueStmt BridgedContinueStmt_createParsed(
    BridgedDeclContext cDeclContext, BridgedSourceLoc cLoc,
    BridgedIdentifier cTargetName, BridgedSourceLoc cTargetLoc);

SWIFT_NAME("BridgedDeferStmt.createParsed(_:deferLoc:)")
BridgedDeferStmt BridgedDeferStmt_createParsed(BridgedDeclContext cDeclContext,
                                               BridgedSourceLoc cDeferLoc);

SWIFT_NAME("getter:BridgedDeferStmt.tempDecl(self:)")
BridgedFuncDecl BridgedDeferStmt_getTempDecl(BridgedDeferStmt bridged);

SWIFT_NAME("BridgedDiscardStmt.createParsed(_:discardLoc:subExpr:)")
BridgedDiscardStmt BridgedDiscardStmt_createParsed(BridgedASTContext cContext,
                                                   BridgedSourceLoc cDiscardLoc,
                                                   BridgedExpr cSubExpr);

SWIFT_NAME("BridgedDoStmt.createParsed(_:labelInfo:doLoc:body:)")
BridgedDoStmt BridgedDoStmt_createParsed(BridgedASTContext cContext,
                                         BridgedLabeledStmtInfo cLabelInfo,
                                         BridgedSourceLoc cDoLoc,
                                         BridgedBraceStmt cBody);

SWIFT_NAME(
    "BridgedDoCatchStmt.createParsed(_:labelInfo:doLoc:throwsLoc:thrownType:"
    "body:catches:)")
BridgedDoCatchStmt BridgedDoCatchStmt_createParsed(
    BridgedDeclContext cDeclContext, BridgedLabeledStmtInfo cLabelInfo,
    BridgedSourceLoc cDoLoc, BridgedSourceLoc cThrowsLoc,
    BridgedNullableTypeRepr cThrownType, BridgedStmt cBody,
    BridgedArrayRef cCatches);

SWIFT_NAME("BridgedFallthroughStmt.createParsed(loc:declContext:)")
BridgedFallthroughStmt
BridgedFallthroughStmt_createParsed(BridgedSourceLoc cLoc,
                                    BridgedDeclContext cDC);

SWIFT_NAME("BridgedForEachStmt.createParsed(_:labelInfo:forLoc:tryLoc:awaitLoc:"
           "pattern:inLoc:sequence:whereLoc:whereExpr:body:)")
BridgedForEachStmt BridgedForEachStmt_createParsed(
    BridgedASTContext cContext, BridgedLabeledStmtInfo cLabelInfo,
    BridgedSourceLoc cForLoc, BridgedSourceLoc cTryLoc,
    BridgedSourceLoc cAwaitLoc, BridgedPattern cPat, BridgedSourceLoc cInLoc,
    BridgedExpr cSequence, BridgedSourceLoc cWhereLoc,
    BridgedNullableExpr cWhereExpr, BridgedBraceStmt cBody);

SWIFT_NAME("BridgedGuardStmt.createParsed(_:guardLoc:conds:body:)")
BridgedGuardStmt BridgedGuardStmt_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cGuardLoc,
                                               BridgedArrayRef cConds,
                                               BridgedBraceStmt cBody);

SWIFT_NAME("BridgedIfStmt.createParsed(_:labelInfo:ifLoc:conditions:then:"
           "elseLoc:else:)")
BridgedIfStmt BridgedIfStmt_createParsed(
    BridgedASTContext cContext, BridgedLabeledStmtInfo cLabelInfo,
    BridgedSourceLoc cIfLoc, BridgedArrayRef cConds, BridgedBraceStmt cThen,
    BridgedSourceLoc cElseLoc, BridgedNullableStmt cElse);

SWIFT_NAME("BridgedRepeatWhileStmt.createParsed(_:labelInfo:repeatLoc:cond:"
           "whileLoc:body:)")
BridgedRepeatWhileStmt BridgedRepeatWhileStmt_createParsed(
    BridgedASTContext cContext, BridgedLabeledStmtInfo cLabelInfo,
    BridgedSourceLoc cRepeatLoc, BridgedExpr cCond, BridgedSourceLoc cWhileLoc,
    BridgedStmt cBody);

SWIFT_NAME("BridgedReturnStmt.createParsed(_:loc:expr:)")
BridgedReturnStmt BridgedReturnStmt_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cLoc,
                                                 BridgedNullableExpr expr);

SWIFT_NAME("BridgedSwitchStmt.createParsed(_:labelInfo:switchLoc:subjectExpr:"
           "lBraceLoc:cases:rBraceLoc:)")
BridgedSwitchStmt BridgedSwitchStmt_createParsed(
    BridgedASTContext cContext, BridgedLabeledStmtInfo cLabelInfo,
    BridgedSourceLoc cSwitchLoc, BridgedExpr cSubjectExpr,
    BridgedSourceLoc cLBraceLoc, BridgedArrayRef cCases,
    BridgedSourceLoc cRBraceLoc);

SWIFT_NAME("BridgedThenStmt.createParsed(_:thenLoc:result:)")
BridgedThenStmt BridgedThenStmt_createParsed(BridgedASTContext cContext,
                                             BridgedSourceLoc cThenLoc,
                                             BridgedExpr cResult);

SWIFT_NAME("BridgedThrowStmt.createParsed(_:throwLoc:subExpr:)")
BridgedThrowStmt BridgedThrowStmt_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cThrowLoc,
                                               BridgedExpr cSubExpr);

SWIFT_NAME("BridgedWhileStmt.createParsed(_:labelInfo:whileLoc:cond:body:)")
BridgedWhileStmt BridgedWhileStmt_createParsed(
    BridgedASTContext cContext, BridgedLabeledStmtInfo cLabelInfo,
    BridgedSourceLoc cWhileLoc, BridgedArrayRef cCond, BridgedStmt cBody);

SWIFT_NAME(
    "BridgedYieldStmt.createParsed(_:yieldLoc:lParenLoc:yields:rParenLoc:)")
BridgedYieldStmt BridgedYieldStmt_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cYieldLoc,
                                               BridgedSourceLoc cLParenLoc,
                                               BridgedArrayRef cYields,
                                               BridgedSourceLoc cRParenLoc);

SWIFT_NAME("BridgedStmt.dump(self:)")
void BridgedStmt_dump(BridgedStmt statement);

//===----------------------------------------------------------------------===//
// MARK: TypeAttributes
//===----------------------------------------------------------------------===//

#ifdef USED_IN_CPP_SOURCE
namespace swift {
class TypeAttributes {
public:
  SmallVector<TypeOrCustomAttr> attrs;
  TypeAttributes() {}
};
} // namespace swift
#endif

// Bridged type attribute kinds, which mirror TypeAttrKind exactly.
enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedTypeAttrKind {
#define TYPE_ATTR(_, CLASS) BridgedTypeAttrKind##CLASS,
#include "swift/AST/TypeAttr.def"
  BridgedTypeAttrKindNone,
};

SWIFT_NAME("BridgedTypeAttrKind.init(from:)")
BridgedTypeAttrKind BridgedTypeAttrKind_fromString(BridgedStringRef cStr);

SWIFT_NAME("BridgedTypeAttributes.new()")
BridgedTypeAttributes BridgedTypeAttributes_create();

SWIFT_NAME("BridgedTypeAttributes.delete(self:)")
void BridgedTypeAttributes_delete(BridgedTypeAttributes cAttributes);

SWIFT_NAME("BridgedTypeAttributes.add(self:_:)")
void BridgedTypeAttributes_add(BridgedTypeAttributes cAttributes,
                               BridgedTypeAttribute cAttribute);

SWIFT_NAME("getter:BridgedTypeAttributes.isEmpty(self:)")
bool BridgedTypeAttributes_isEmpty(BridgedTypeAttributes cAttributes);

SWIFT_NAME("BridgedTypeAttribute.createSimple(_:kind:atLoc:nameLoc:)")
BridgedTypeAttribute BridgedTypeAttribute_createSimple(
    BridgedASTContext cContext, BridgedTypeAttrKind cKind,
    BridgedSourceLoc cAtLoc, BridgedSourceLoc cNameLoc);

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedIsolatedTypeAttrIsolationKind {
  BridgedIsolatedTypeAttrIsolationKind_DynamicIsolation,
};

SWIFT_NAME("BridgedTypeAttribute.createIsolated(_:atLoc:nameLoc:lpLoc:isolationKindLoc:isolationKind:rpLoc:)")
BridgedTypeAttribute BridgedTypeAttribute_createIsolated(
    BridgedASTContext cContext,
    BridgedSourceLoc cAtLoc, BridgedSourceLoc cNameLoc,
    BridgedSourceLoc cLPLoc, BridgedSourceLoc cIsolationLoc,
    BridgedIsolatedTypeAttrIsolationKind cIsolation, BridgedSourceLoc cRPLoc);

//===----------------------------------------------------------------------===//
// MARK: TypeReprs
//===----------------------------------------------------------------------===//

/// Bridged parameter specifiers
enum ENUM_EXTENSIBILITY_ATTR(open) BridgedAttributedTypeSpecifier : size_t {
  BridgedAttributedTypeSpecifierInOut,
  BridgedAttributedTypeSpecifierBorrowing,
  BridgedAttributedTypeSpecifierConsuming,
  BridgedAttributedTypeSpecifierLegacyShared,
  BridgedAttributedTypeSpecifierLegacyOwned,
  BridgedAttributedTypeSpecifierConst,
  BridgedAttributedTypeSpecifierIsolated,
  BridgedAttributedTypeSpecifierSending,
};

SWIFT_NAME("BridgedUnqualifiedIdentTypeRepr.createParsed(_:loc:name:)")
BridgedUnqualifiedIdentTypeRepr BridgedUnqualifiedIdentTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLoc, BridgedIdentifier id);

SWIFT_NAME(
    "BridgedSpecifierTypeRepr.createParsed(_:base:specifier:specifierLoc:)")
BridgedSpecifierTypeRepr BridgedSpecifierTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base,
    BridgedAttributedTypeSpecifier specifier, BridgedSourceLoc cSpecifierLoc);

SWIFT_NAME(
    "BridgedArrayTypeRepr.createParsed(_:base:leftSquareLoc:rightSquareLoc:)")
BridgedArrayTypeRepr BridgedArrayTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base,
    BridgedSourceLoc cLSquareLoc, BridgedSourceLoc cRSquareLoc);

SWIFT_NAME(
    "BridgedAttributedTypeRepr.createParsed(_:base:consumingAttributes:)")
BridgedAttributedTypeRepr
BridgedAttributedTypeRepr_createParsed(BridgedASTContext cContext,
                                       BridgedTypeRepr base,
                                       BridgedTypeAttributes cAttributes);

SWIFT_NAME("BridgedCompositionTypeRepr.createEmpty(_:anyKeywordLoc:)")
BridgedCompositionTypeRepr
BridgedCompositionTypeRepr_createEmpty(BridgedASTContext cContext,
                                       BridgedSourceLoc cAnyLoc);

SWIFT_NAME("BridgedCompositionTypeRepr.createParsed(_:types:ampersandLoc:)")
BridgedCompositionTypeRepr
BridgedCompositionTypeRepr_createParsed(BridgedASTContext cContext,
                                        BridgedArrayRef types,
                                        BridgedSourceLoc cFirstAmpLoc);

SWIFT_NAME("BridgedDictionaryTypeRepr.createParsed(_:leftSquareLoc:keyType:"
           "colonLoc:valueType:rightSquareLoc:)")
BridgedDictionaryTypeRepr BridgedDictionaryTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLSquareLoc,
    BridgedTypeRepr keyType, BridgedSourceLoc cColonloc,
    BridgedTypeRepr valueType, BridgedSourceLoc cRSquareLoc);

SWIFT_NAME("BridgedFunctionTypeRepr.createParsed(_:argsType:asyncLoc:throwsLoc:"
           "thrownType:arrowLoc:resultType:)")
BridgedFunctionTypeRepr BridgedFunctionTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr argsTy,
    BridgedSourceLoc cAsyncLoc, BridgedSourceLoc cThrowsLoc,
    BridgedNullableTypeRepr thrownType, BridgedSourceLoc cArrowLoc,
    BridgedTypeRepr resultType);

SWIFT_NAME("BridgedUnqualifiedIdentTypeRepr.createParsed(_:name:nameLoc:"
           "genericArgs:leftAngleLoc:rightAngleLoc:)")
BridgedUnqualifiedIdentTypeRepr BridgedUnqualifiedIdentTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedIdentifier name,
    BridgedSourceLoc cNameLoc, BridgedArrayRef genericArgs,
    BridgedSourceLoc cLAngleLoc, BridgedSourceLoc cRAngleLoc);

SWIFT_NAME("BridgedOptionalTypeRepr.createParsed(_:base:questionLoc:)")
BridgedOptionalTypeRepr
BridgedOptionalTypeRepr_createParsed(BridgedASTContext cContext,
                                     BridgedTypeRepr base,
                                     BridgedSourceLoc cQuestionLoc);

SWIFT_NAME("BridgedImplicitlyUnwrappedOptionalTypeRepr.createParsed(_:base:"
           "exclaimLoc:)")
BridgedImplicitlyUnwrappedOptionalTypeRepr
BridgedImplicitlyUnwrappedOptionalTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr base,
    BridgedSourceLoc cExclamationLoc);

SWIFT_NAME("BridgedInverseTypeRepr.createParsed(_:tildeLoc:constraint:)")
BridgedInverseTypeRepr
BridgedInverseTypeRepr_createParsed(BridgedASTContext cContext,
                                    BridgedSourceLoc cTildeLoc,
                                    BridgedTypeRepr cConstraint);

SWIFT_NAME("BridgedDeclRefTypeRepr.createParsed(_:base:name:nameLoc:genericArguments:angleRange:)")
BridgedDeclRefTypeRepr BridgedDeclRefTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedTypeRepr cBase, BridgedIdentifier cName,
    BridgedSourceLoc cLoc, BridgedArrayRef cGenericArguments,
    BridgedSourceRange cAngleRange);

SWIFT_NAME("BridgedMetatypeTypeRepr.createParsed(_:base:typeKeywordLoc:)")
BridgedMetatypeTypeRepr
BridgedMetatypeTypeRepr_createParsed(BridgedASTContext cContext,
                                     BridgedTypeRepr baseType,
                                     BridgedSourceLoc cTypeLoc);

SWIFT_NAME("BridgedProtocolTypeRepr.createParsed(_:base:protocolKeywordLoc:)")
BridgedProtocolTypeRepr
BridgedProtocolTypeRepr_createParsed(BridgedASTContext cContext,
                                     BridgedTypeRepr baseType,
                                     BridgedSourceLoc cProtoLoc);

SWIFT_NAME("BridgedPackElementTypeRepr.createParsed(_:base:eachKeywordLoc:)")
BridgedPackElementTypeRepr
BridgedPackElementTypeRepr_createParsed(BridgedASTContext cContext,
                                        BridgedTypeRepr base,
                                        BridgedSourceLoc cEachLoc);

SWIFT_NAME(
    "BridgedPackExpansionTypeRepr.createParsed(_:base:repeatKeywordLoc:)")
BridgedPackExpansionTypeRepr
BridgedPackExpansionTypeRepr_createParsed(BridgedASTContext cContext,
                                          BridgedTypeRepr base,
                                          BridgedSourceLoc cRepeatLoc);

SWIFT_NAME(
    "BridgedTupleTypeRepr.createParsed(_:elements:leftParenLoc:rightParenLoc:)")
BridgedTupleTypeRepr BridgedTupleTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedArrayRef elements,
    BridgedSourceLoc cLParenLoc, BridgedSourceLoc cRParenLoc);

SWIFT_NAME("BridgedNamedOpaqueReturnTypeRepr.createParsed(_:base:)")
BridgedNamedOpaqueReturnTypeRepr
BridgedNamedOpaqueReturnTypeRepr_createParsed(BridgedASTContext cContext,
                                              BridgedTypeRepr baseTy);

SWIFT_NAME("BridgedOpaqueReturnTypeRepr.createParsed(_:someKeywordLoc:base:)")
BridgedOpaqueReturnTypeRepr
BridgedOpaqueReturnTypeRepr_createParsed(BridgedASTContext cContext,
                                         BridgedSourceLoc cOpaqueLoc,
                                         BridgedTypeRepr baseTy);

SWIFT_NAME("BridgedExistentialTypeRepr.createParsed(_:anyKeywordLoc:base:)")
BridgedExistentialTypeRepr
BridgedExistentialTypeRepr_createParsed(BridgedASTContext cContext,
                                        BridgedSourceLoc cAnyLoc,
                                        BridgedTypeRepr baseTy);

SWIFT_NAME("BridgedVarargTypeRepr.createParsed(_:base:ellipsisLoc:)")
BridgedVarargTypeRepr
BridgedVarargTypeRepr_createParsed(BridgedASTContext cContext,
                                   BridgedTypeRepr base,
                                   BridgedSourceLoc cEllipsisLoc);

SWIFT_NAME(
    "BridgedIntegerTypeRepr.createParsed(_:string:loc:minusLoc:)")
BridgedIntegerTypeRepr BridgedIntegerTypeRepr_createParsed(
    BridgedASTContext cContext, BridgedStringRef cString, BridgedSourceLoc cLoc,
    BridgedSourceLoc cMinusLoc);

SWIFT_NAME("BridgedTypeRepr.dump(self:)")
void BridgedTypeRepr_dump(BridgedTypeRepr type);

//===----------------------------------------------------------------------===//
// MARK: Patterns
//===----------------------------------------------------------------------===//

SWIFT_NAME("getter:BridgedPattern.singleVar(self:)")
BridgedNullableVarDecl BridgedPattern_getSingleVar(BridgedPattern cPattern);

SWIFT_NAME("BridgedAnyPattern.createParsed(_:loc:)")
BridgedAnyPattern BridgedAnyPattern_createParsed(BridgedASTContext cContext,
                                                 BridgedSourceLoc cLoc);

SWIFT_NAME("BridgedBindingPattern.createParsed(_:keywordLoc:isLet:subPattern:)")
BridgedBindingPattern
BridgedBindingPattern_createParsed(BridgedASTContext cContext,
                                   BridgedSourceLoc cKeywordLoc, bool isLet,
                                   BridgedPattern cSubPattern);

SWIFT_NAME("BridgedBindingPattern.createImplicitCatch(_:loc:)")
BridgedBindingPattern
BridgedBindingPattern_createImplicitCatch(BridgedDeclContext cDeclContext,
                                          BridgedSourceLoc cLoc);

SWIFT_NAME("BridgedExprPattern.createParsed(_:expr:)")
BridgedExprPattern
BridgedExprPattern_createParsed(BridgedDeclContext cDeclContext,
                                BridgedExpr cExpr);

SWIFT_NAME("BridgedIsPattern.createParsed(_:isLoc:typeExpr:)")
BridgedIsPattern BridgedIsPattern_createParsed(BridgedASTContext cContext,
                                               BridgedSourceLoc cIsLoc,
                                               BridgedTypeExpr cTypeExpr);

SWIFT_NAME("BridgedNamedPattern.createParsed(_:declContext:name:loc:)")
BridgedNamedPattern
BridgedNamedPattern_createParsed(BridgedASTContext astContext,
                                 BridgedDeclContext declContext,
                                 BridgedIdentifier name, BridgedSourceLoc cLoc);

SWIFT_NAME(
    "BridgedParenPattern.createParsed(_:lParenLoc:subPattern:rParenLoc:)")
BridgedParenPattern BridgedParenPattern_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLParenLoc,
    BridgedPattern cSubPattern, BridgedSourceLoc cRParenLoc);

struct BridgedTuplePatternElt {
  BridgedIdentifier Label;
  BridgedSourceLoc LabelLoc;
  BridgedPattern ThePattern;
};

SWIFT_NAME("BridgedTuplePattern.createParsed(_:lParenLoc:elements:rParenLoc:)")
BridgedTuplePattern BridgedTuplePattern_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLParenLoc,
    BridgedArrayRef cElements, BridgedSourceLoc cRParenLoc);

SWIFT_NAME("BridgedTypedPattern.createParsed(_:pattern:type:)")
BridgedTypedPattern BridgedTypedPattern_createParsed(BridgedASTContext cContext,
                                                     BridgedPattern cPattern,
                                                     BridgedTypeRepr cType);

SWIFT_NAME("BridgedTypedPattern.createPropagated(_:pattern:type:)")
BridgedTypedPattern BridgedTypedPattern_createPropagated(
    BridgedASTContext cContext, BridgedPattern cPattern, BridgedTypeRepr cType);

SWIFT_NAME("BridgedPattern.setImplicit(self:)")
void BridgedPattern_setImplicit(BridgedPattern cPattern);

SWIFT_NAME("getter:BridgedPattern.boundName(self:)")
BridgedIdentifier BridgedPattern_getBoundName(BridgedPattern cPattern);

//===----------------------------------------------------------------------===//
// MARK: Misc
//===----------------------------------------------------------------------===//

struct BridgedTupleTypeElement {
  BridgedIdentifier Name;
  BridgedSourceLoc NameLoc;
  BridgedIdentifier SecondName;
  BridgedSourceLoc SecondNameLoc;
  BridgedSourceLoc UnderscoreLoc;
  BridgedSourceLoc ColonLoc;
  BridgedTypeRepr Type;
  BridgedSourceLoc TrailingCommaLoc;
};

enum ENUM_EXTENSIBILITY_ATTR(open) BridgedRequirementReprKind : size_t {
  /// A type bound T : P, where T is a type that depends on a generic
  /// parameter and P is some type that should bound T, either as a concrete
  /// supertype or a protocol to which T must conform.
  BridgedRequirementReprKindTypeConstraint,

  /// A same-type requirement T == U, where T and U are types that shall be
  /// equivalent.
  BridgedRequirementReprKindSameType,

  /// A layout bound T : L, where T is a type that depends on a generic
  /// parameter and L is some layout specification that should bound T.
  BridgedRequirementReprKindLayoutConstraint,

  // Note: there is code that packs this enum in a 2-bit bitfield.  Audit users
  // when adding enumerators.
};

struct BridgedRequirementRepr {
  BridgedSourceLoc SeparatorLoc;
  BridgedRequirementReprKind Kind;
  BridgedTypeRepr FirstType;
  BridgedTypeRepr SecondType;
  // FIXME: Handle Layout Requirements
};

enum ENUM_EXTENSIBILITY_ATTR(open) BridgedMacroDefinitionKind : size_t {
  /// An expanded macro.
  BridgedExpandedMacro = 0,
  /// An external macro, spelled with either the old spelling (Module.Type)
  /// or the new spelling `#externalMacro(module: "Module", type: "Type")`.
  BridgedExternalMacro,
  /// The builtin definition for "externalMacro".
  BridgedBuiltinExternalMacro,
  /// The builtin definition for the "isolation" macro.
  BridgedBuiltinIsolationMacro,
};

enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedGenericTypeParamKind : size_t {
  /// A regular generic type parameter: 'T'
  BridgedGenericTypeParamKindType = 0,
  /// A generic parameter pack: 'each T'
  BridgedGenericTypeParamKindPack,
  /// A generic value parameter: 'let T'
  BridgedGenericTypeParamKindValue,
};

SWIFT_NAME("BridgedGenericParamList.createParsed(_:leftAngleLoc:parameters:"
           "genericWhereClause:rightAngleLoc:)")
BridgedGenericParamList BridgedGenericParamList_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLeftAngleLoc,
    BridgedArrayRef cParameters,
    BridgedNullableTrailingWhereClause genericWhereClause,
    BridgedSourceLoc cRightAngleLoc);

SWIFT_NAME(
    "BridgedGenericTypeParamDecl.createParsed(_:declContext:specifierLoc:"
    "name:nameLoc:inheritedType:index:paramKind:)")
BridgedGenericTypeParamDecl BridgedGenericTypeParamDecl_createParsed(
    BridgedASTContext cContext, BridgedDeclContext cDeclContext,
    BridgedSourceLoc cSpecifierLoc, BridgedIdentifier cName,
    BridgedSourceLoc cNameLoc, BridgedNullableTypeRepr opaqueInheritedType,
    size_t index, BridgedGenericTypeParamKind paramKind);

SWIFT_NAME(
    "BridgedTrailingWhereClause.createParsed(_:whereKeywordLoc:requirements:)")
BridgedTrailingWhereClause
BridgedTrailingWhereClause_createParsed(BridgedASTContext cContext,
                                        BridgedSourceLoc cWhereKeywordLoc,
                                        BridgedArrayRef cRequirements);

SWIFT_NAME("BridgedParameterList.createParsed(_:leftParenLoc:parameters:"
           "rightParenLoc:)")
BridgedParameterList BridgedParameterList_createParsed(
    BridgedASTContext cContext, BridgedSourceLoc cLeftParenLoc,
    BridgedArrayRef cParameters, BridgedSourceLoc cRightParenLoc);

struct BridgedASTType {
  swift::TypeBase * _Nullable type;

  BRIDGED_INLINE swift::Type unbridged() const;
  BRIDGED_INLINE BridgedOwnedString getDebugDescription() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedCanType getCanonicalType() const;
  BRIDGED_INLINE bool hasTypeParameter() const;
  BRIDGED_INLINE bool isOpenedExistentialWithError() const;
  BRIDGED_INLINE bool isEscapable() const;
  BRIDGED_INLINE bool isNoEscape() const;
  BRIDGED_INLINE bool isInteger() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedASTType subst(BridgedSubstitutionMap substMap) const;
};

class BridgedCanType {
  swift::TypeBase * _Nullable type;

public:
  BRIDGED_INLINE BridgedCanType(swift::CanType ty);
  BRIDGED_INLINE swift::CanType unbridged() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedASTType getType() const;
};

struct BridgedConformance {
  void * _Nullable opaqueValue;

  BRIDGED_INLINE BridgedConformance(swift::ProtocolConformanceRef conformance);
  BRIDGED_INLINE swift::ProtocolConformanceRef unbridged() const;

  BridgedOwnedString getDebugDescription() const;
  BRIDGED_INLINE bool isConcrete() const;
  BRIDGED_INLINE bool isValid() const;
  BRIDGED_INLINE bool isSpecializedConformance() const;
  BRIDGED_INLINE bool isInheritedConformance() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedASTType getType() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedConformance getGenericConformance() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedConformance getInheritedConformance() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedSubstitutionMap getSpecializedSubstitutions() const;
};

struct BridgedConformanceArray {
  BridgedArrayRef pcArray;

  SwiftInt getCount() const { return SwiftInt(pcArray.Length); }

  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE
  BridgedConformance getAt(SwiftInt index) const;
};

struct BridgedSubstitutionMap {
  uint64_t storage[1];

  BRIDGED_INLINE BridgedSubstitutionMap(swift::SubstitutionMap map);
  BRIDGED_INLINE swift::SubstitutionMap unbridged() const;
  BRIDGED_INLINE BridgedSubstitutionMap();
  BridgedOwnedString getDebugDescription() const;
  BRIDGED_INLINE bool isEmpty() const;
  BRIDGED_INLINE bool hasAnySubstitutableParams() const;
  BRIDGED_INLINE SwiftInt getNumConformances() const;
  SWIFT_IMPORT_UNSAFE BRIDGED_INLINE BridgedConformance getConformance(SwiftInt index) const;
};

struct BridgedFingerprint {
  uint64_t v1;
  uint64_t v2;

  BRIDGED_INLINE swift::Fingerprint unbridged() const;
};

//===----------------------------------------------------------------------===//
// MARK: #if handling
//===----------------------------------------------------------------------===//

/// Bridged version of IfConfigClauseRangeInfo::ClauseKind.
enum ENUM_EXTENSIBILITY_ATTR(closed) BridgedIfConfigClauseKind : size_t {
  IfConfigActive,
  IfConfigInactive,
  IfConfigEnd
};

/// Bridged version of IfConfigClauseRangeInfo.
struct BridgedIfConfigClauseRangeInfo {
  BridgedSourceLoc directiveLoc;
  BridgedSourceLoc bodyLoc;
  BridgedSourceLoc endLoc;
  BridgedIfConfigClauseKind kind;

  BRIDGED_INLINE swift::IfConfigClauseRangeInfo unbridged() const;
};

//===----------------------------------------------------------------------===//
// MARK: Plugins
//===----------------------------------------------------------------------===//

SWIFT_BEGIN_ASSUME_NONNULL

typedef void *PluginHandle;
typedef const void *PluginCapabilityPtr;

/// Set a capability data to the plugin object. Since the data is just a opaque
/// pointer, it's not used in AST at all.
void Plugin_setCapability(PluginHandle handle,
                          PluginCapabilityPtr _Nullable data);

/// Get a capability data set by \c Plugin_setCapability .
PluginCapabilityPtr _Nullable Plugin_getCapability(PluginHandle handle);

/// Lock the plugin. Clients should lock it during sending and recving the
/// response.
void Plugin_lock(PluginHandle handle);

/// Unlock the plugin.
void Plugin_unlock(PluginHandle handle);

/// Launch the plugin if it's not running.
bool Plugin_spawnIfNeeded(PluginHandle handle);

/// Sends the message to the plugin, returns true if there was an error.
/// Clients should receive the response  by \c Plugin_waitForNextMessage .
bool Plugin_sendMessage(PluginHandle handle, const BridgedData data);

/// Receive a message from the plugin.
bool Plugin_waitForNextMessage(PluginHandle handle, BridgedData *data);

SWIFT_END_ASSUME_NONNULL

SWIFT_END_NULLABILITY_ANNOTATIONS

#ifndef PURE_BRIDGING_MODE
// In _not_ PURE_BRIDGING_MODE, bridging functions are inlined and therefore
// included in the header file. This is because they rely on C++ headers that
// we don't want to pull in when using "pure bridging mode".
#include "ASTBridgingImpl.h"
#endif

#endif // SWIFT_AST_ASTBRIDGING_H
