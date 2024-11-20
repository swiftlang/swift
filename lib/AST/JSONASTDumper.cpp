//===--- JSONASTDumper.cpp - Swift Language JSON AST Dumper ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements JSON-formatted dumping for the Swift ASTs.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTNode.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/PackConformance.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Version.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

namespace {

template <typename T, typename = void>
struct is_iterable : std::false_type {};

template <typename T>
struct is_iterable<T, std::void_t<decltype(std::begin(std::declval<T &>())),
                                  decltype(std::end(std::declval<T &>()))>>
    : std::true_type {};

/// Returns the USR of the given declaration. Gracefully returns an empty
/// string if D is null or invalid.
std::string declUSR(const Decl *D) {
  if (!D)
    return "";

  // Certain local synthesized declarations won't be assigned a local
  // discriminator, later causing an assertion if we try to generate a USR
  // for them. Avoid these.
  if (auto VD = dyn_cast<ValueDecl>(D);
      VD && VD->getDeclContext()->isLocalContext() &&
      (!VD->getLoc().isValid() ||
       (VD->getModuleContext()
            ->getSourceFileContainingLocation(VD->getLoc())
            ->getFulfilledMacroRole() == std::nullopt))) {
    return "";
  }

  std::string usr;
  llvm::raw_string_ostream os(usr);
  if (swift::ide::printDeclUSR(D, os))
    return "";
  return usr;
}

/// Returns a vector of USRs from a sequence of declarations.
template <typename T>
std::vector<std::string> declUSRs(const T &decls) {
  std::vector<std::string> result;
  for (auto d : decls)
    result.push_back(declUSR(d));
  return result;
}

/// Returns the USR of the given type. Gracefully returns an empty string
/// if the type is invalid.
std::string typeUSR(Type type) {
  if (!type)
    return "";

  if (type->hasArchetype()) {
    // We can't generate USRs for types that contain archetypes. Replace them
    // with their interface types.
    type = type.transformRec([&](TypeBase *t) -> std::optional<Type> {
      if (auto AT = dyn_cast<ArchetypeType>(t)) {
        return AT->getInterfaceType();
      }
      return std::nullopt;
    });
  }

  std::string usr;
  llvm::raw_string_ostream os(usr);
  if (swift::ide::printTypeUSR(type, os))
    return "";
  return usr;
}

/// Returns the USR of the given value declaration's type. Gracefully returns
/// the empty string if D is null.
std::string declTypeUSR(const ValueDecl *D) {
  if (!D)
    return "";

  std::string usr;
  llvm::raw_string_ostream os(usr);
  if (swift::ide::printDeclTypeUSR(D, os))
    return "";
  return usr;
}

std::string jsonStringForAccessorKind(AccessorKind kind) {
  switch (kind) {
  case AccessorKind::Get:
    return "get";
  case AccessorKind::DistributedGet:
    return "_distributed_get";
  case AccessorKind::Set:
    return "set";
  case AccessorKind::Read:
    return "_read";
  case AccessorKind::Read2:
    return "read";
  case AccessorKind::Modify:
    return "_modify";
  case AccessorKind::Modify2:
    return "modify";
  case AccessorKind::WillSet:
    return "willSet";
  case AccessorKind::DidSet:
    return "didSet";
  case AccessorKind::Address:
    return "unsafeAddress";
  case AccessorKind::MutableAddress:
    return "unsafeMutableAddress";
  case AccessorKind::Init:
    return "init";
  }
}

std::string jsonStringForAssociativity(Associativity assoc) {
  switch (assoc) {
  case Associativity::None:
    return "none";
  case Associativity::Left:
    return "left";
  case Associativity::Right:
    return "right";
  }
}

std::string jsonStringForAvailabilitySpecKind(AvailabilitySpecKind kind) {
  switch (kind) {
  case AvailabilitySpecKind::PlatformVersionConstraint:
    return "platformVersion";
  case AvailabilitySpecKind::OtherPlatform:
    return "otherPlatform";
  case AvailabilitySpecKind::LanguageVersionConstraint:
    return "languageVersion";
  case AvailabilitySpecKind::PackageDescriptionVersionConstraint:
    return "packageDescription";
  }
}

std::string jsonStringForCtorInitializerKind(CtorInitializerKind kind) {
  switch (kind) {
  case CtorInitializerKind::Convenience:
    return "convenience";
  case CtorInitializerKind::ConvenienceFactory:
    return "convenienceFactory";
  case CtorInitializerKind::Designated:
    return "designated";
  case CtorInitializerKind::Factory:
    return "factory";
  }
}

std::string jsonStringForGenericTypeParamKind(GenericTypeParamKind kind) {
  switch (kind) {
  case GenericTypeParamKind::Type:
    return "type";
  case GenericTypeParamKind::Pack:
    return "pack";
  case GenericTypeParamKind::Value:
    return "value";
  }
}

std::string jsonStringForImportKind(ImportKind kind) {
  switch (kind) {
  case ImportKind::Module:
    return "module";
  case ImportKind::Type:
    return "type";
  case ImportKind::Struct:
    return "struct";
  case ImportKind::Class:
    return "class";
  case ImportKind::Enum:
    return "enum";
  case ImportKind::Protocol:
    return "protocol";
  case ImportKind::Var:
    return "var";
  case ImportKind::Func:
    return "func";
  }
}

std::string
jsonStringForKeyPathExprComponentKind(KeyPathExpr::Component::Kind kind) {
  switch (kind) {
  case KeyPathExpr::Component::Kind::Invalid:
    return "invalid";
  case KeyPathExpr::Component::Kind::UnresolvedProperty:
    return "unresolvedProperty";
  case KeyPathExpr::Component::Kind::UnresolvedSubscript:
    return "unresolvedSubscript";
  case KeyPathExpr::Component::Kind::Property:
    return "property";
  case KeyPathExpr::Component::Kind::Subscript:
    return "subscript";
  case KeyPathExpr::Component::Kind::OptionalForce:
    return "optionalForce";
  case KeyPathExpr::Component::Kind::OptionalChain:
    return "optionalChain";
  case KeyPathExpr::Component::Kind::OptionalWrap:
    return "optionalWrap";
  case KeyPathExpr::Component::Kind::Identity:
    return "identity";
  case KeyPathExpr::Component::Kind::TupleElement:
    return "tupleElement";
  case KeyPathExpr::Component::Kind::DictionaryKey:
    return "dictionaryKey";
  case KeyPathExpr::Component::Kind::CodeCompletion:
    return "codeCompletion";
  }
}

std::string jsonStringForLayoutConstraintKind(LayoutConstraintKind kind) {
  switch (kind) {
  case LayoutConstraintKind::UnknownLayout:
    return "unknown";
  case LayoutConstraintKind::TrivialOfExactSize:
    return "trivialOfExactSize";
  case LayoutConstraintKind::TrivialOfAtMostSize:
    return "trivialOfAtMostSize";
  case LayoutConstraintKind::Trivial:
    return "trivial";
  case LayoutConstraintKind::Class:
    return "class";
  case LayoutConstraintKind::NativeClass:
    return "nativeClass";
  case LayoutConstraintKind::RefCountedObject:
    return "refCountedObject";
  case LayoutConstraintKind::NativeRefCountedObject:
    return "nativeRefCountedObject";
  case LayoutConstraintKind::BridgeObject:
    return "bridgeObject";
  case LayoutConstraintKind::TrivialStride:
    return "trivialStride";
  }
}

std::string jsonStringForPlatformAgnosticAvailabilityKind(
    PlatformAgnosticAvailabilityKind kind) {
  switch (kind) {
  case PlatformAgnosticAvailabilityKind::None:
    return "";
  case PlatformAgnosticAvailabilityKind::Deprecated:
    return "deprecated";
  case PlatformAgnosticAvailabilityKind::UnavailableInSwift:
    return "unavailableInSwift";
  case PlatformAgnosticAvailabilityKind::SwiftVersionSpecific:
    return "swiftVersionSpecific";
  case PlatformAgnosticAvailabilityKind::PackageDescriptionVersionSpecific:
    return "packageDescriptionVersionSpecific";
  case PlatformAgnosticAvailabilityKind::Unavailable:
    return "unavailable";
  case PlatformAgnosticAvailabilityKind::NoAsync:
    return "noAsync";
  }
}

std::string jsonStringForPropertyWrapperValueKind(
    AppliedPropertyWrapperExpr::ValueKind kind) {
  switch (kind) {
  case AppliedPropertyWrapperExpr::ValueKind::WrappedValue:
    return "wrappedValue";
  case AppliedPropertyWrapperExpr::ValueKind::ProjectedValue:
    return "projectedValue";
  }
}

std::string jsonStringForReadImplKind(ReadImplKind kind) {
  switch (kind) {
  case ReadImplKind::Stored:
    return "stored";
  case ReadImplKind::Get:
    return "get";
  case ReadImplKind::Inherited:
    return "inherited";
  case ReadImplKind::Address:
    return "address";
  case ReadImplKind::Read:
    return "_read";
  case ReadImplKind::Read2:
    return "read";
  }
}

std::string jsonStringForWriteImplKind(WriteImplKind kind) {
  switch (kind) {
  case WriteImplKind::Immutable:
    return "immutable";
  case WriteImplKind::Stored:
    return "stored";
  case WriteImplKind::StoredWithObservers:
    return "storedWitHObservers";
  case WriteImplKind::InheritedWithObservers:
    return "inheritedWithObservers";
  case WriteImplKind::Set:
    return "set";
  case WriteImplKind::MutableAddress:
    return "mutableAddress";
  case WriteImplKind::Modify:
    return "_modify";
  case WriteImplKind::Modify2:
    return "modify";
  }
}

std::string jsonStringForReadWriteImplKind(ReadWriteImplKind kind) {
  switch (kind) {
  case ReadWriteImplKind::Immutable:
    return "immutable";
  case ReadWriteImplKind::Stored:
    return "stored";
  case ReadWriteImplKind::MutableAddress:
    return "mutableAddress";
  case ReadWriteImplKind::MaterializeToTemporary:
    return "materializeToTemporary";
  case ReadWriteImplKind::Modify:
    return "_modify";
  case ReadWriteImplKind::Modify2:
    return "modify";
  case ReadWriteImplKind::StoredWithDidSet:
    return "storedWithDidSet";
  case ReadWriteImplKind::InheritedWithDidSet:
    return "inheritedWithDidSet";
  }
}

std::string jsonStringForRequirementKind(RequirementKind kind) {
  switch (kind) {
  case RequirementKind::Conformance:
    return "conformance";
  case RequirementKind::Superclass:
    return "superclass";
  case RequirementKind::SameType:
    return "sameType";
  case RequirementKind::Layout:
    return "layout";
  case RequirementKind::SameShape:
    return "sameShape";
  }
}

std::string jsonStringForStaticSpellingKind(StaticSpellingKind kind) {
  switch (kind) {
  case StaticSpellingKind::None:
    return "";
  case StaticSpellingKind::KeywordStatic:
    return "static";
  case StaticSpellingKind::KeywordClass:
    return "class";
  }
}

std::string
jsonStringForStmtConditionKind(StmtConditionElement::ConditionKind kind) {
  switch (kind) {
  case StmtConditionElement::CK_Boolean:
    return "boolean";
  case StmtConditionElement::CK_PatternBinding:
    return "pattern";
  case StmtConditionElement::CK_Availability:
    return "availability";
  case StmtConditionElement::CK_HasSymbol:
    return "hasSymbol";
  }
}

/// This class dumps a type-checked Swift AST in JSON format to some output
/// stream (either a file or standard out).
///
/// The only guarantees made for this output are as follows:
///
/// -   The output is valid JSON.
/// -   The top-level object contains a `version` key whose value is an
///     object that contains the keys `major` and `minor` whose values are
///     integers specifying the compiler version used to generate it, as
///     well as the key `full` whose value is a string containing the full
///     compiler version as a string (in the same format that would be
///     written in a .swiftinterface file).
///
/// The Swift compiler may introduce new AST nodes, remove AST nodes, or
/// change the layout of AST nodes at any time, so no stability is implied;
/// the structure and layout of nodes may change between compiler versions.
/// No promise is made that the output contains all possible information
/// about the AST, only that what is presented there is correct. It is also
/// a non-goal for the output to repeat with full fidelity purely syntactic
/// information about nodes (their locations, spelling, etc.) that could be
/// obtained from a syntax parse of the source, although minimal source
/// location information is provided for nodes so that consumers with both
/// the semantic JSON and a syntax tree in hand may relate the information.
///
/// Despite the above warnings, the output presented here is meant to be
/// useful to consumers who wish to perform semantic analysis on Swift
/// sources at a large scale, and this guides how certain data is represented
/// in the dumped AST. For example, references to declarations are expressed
/// as USRs so that this output can be combined with and related to other
/// data sources like indexstore and SourceKit, and types are likewise
/// rendered as USRs since the mangling fully represents the interesting
/// semantic properties and they can be demangled to analyze them further.
class JSONASTVisitor : public ASTVisitor<JSONASTVisitor> {
private:
  llvm::json::OStream json;
  ASTContext &ctx;

public:
  JSONASTVisitor(llvm::raw_ostream &os, ASTContext &ctx)
      : json(os, /*indent=*/0), ctx(ctx) {}

  void visitSourceFile(SourceFile &SF) {
    json.object([&] {
      attributeString("file", SF.getFilename());

      json.attributeObject("version", [&] {
        auto version = version::getSwiftNumericVersion();
        attribute("major", version.first);
        attribute("minor", version.second);
        attributeString("full",
                        version::getSwiftFullVersion(
                            version::Version::getCurrentLanguageVersion()));
      });
      attributeBool("hasEntryPoint", SF.hasEntryPoint());
      attributeString("mainDeclUSR", declUSR(SF.getMainDecl()));
      json.attributeArray("topLevelItems", [&] {
        for (auto item : SF.getTopLevelItems()) {
          value(item);
        }
      });
    });
  }

  // MARK: Declarations

  void visitDecl(Decl *D) {
    attributeString("usr", declUSR(D));
    dumpAttributes(D);
  }

  void dumpAttributes(Decl *D) {
    DeclAttributes &attrs = D->getAttrs();

    attributeBool("isDynamic", attrs.hasAttribute<DynamicAttr>());
    attributeBool("isFinal", attrs.hasAttribute<FinalAttr>());
    attributeBool("isImplementationOnly",
                  attrs.hasAttribute<ImplementationOnlyAttr>());
    attributeBool("isLazy", attrs.hasAttribute<LazyAttr>());
    attributeBool("isKnownToBeLocal", attrs.hasAttribute<KnownToBeLocalAttr>());
    attributeBool("isMainType", attrs.hasAttribute<MainTypeAttr>());
    attributeBool("isPreconcurrency", attrs.hasAttribute<PreconcurrencyAttr>());

    if (auto *availableAttr = attrs.getAttribute<AvailableAttr>()) {
      json.attributeObject("availability", [&] {
        attributeString("message", availableAttr->Message);
        attributeString("rename", availableAttr->Rename);
        attributeString("renameDeclUSR", declUSR(availableAttr->RenameDecl));
        attribute("introduced", availableAttr->Introduced);
        attribute("deprecated", availableAttr->Deprecated);
        attribute("obsoleted", availableAttr->Obsoleted);
        attributeString("platformAgnostic",
                        jsonStringForPlatformAgnosticAvailabilityKind(
                            availableAttr->getPlatformAgnosticAvailability()));
        attributeString("platform", availableAttr->platformString());
        attributeBool("isNoAsync", availableAttr->isNoAsync());
      });
    }
    if (auto *nonisolatedAttr = attrs.getAttribute<NonisolatedAttr>()) {
      json.attributeObject("nonisolated", [&] {
        attributeBool("isUnsafe", nonisolatedAttr->isUnsafe());
      });
    }
    if (auto *objcAttr = attrs.getAttribute<ObjCAttr>()) {
      json.attributeObject("objc",
                           [&] { attribute("name", objcAttr->getName()); });
    }
    if (auto *objcImplAttr = attrs.getAttribute<ObjCImplementationAttr>()) {
      json.attributeObject("objcImplementation", [&] {
        attributeString("categoryName", objcImplAttr->CategoryName.str());
      });
    }

    auto customAttrs = attrs.getAttributes<CustomAttr>();
    if (!customAttrs.empty()) {
      json.attributeArray("customAttrs", [&] {
        for (auto CA : customAttrs) {
          json.object([&] {
            attributeString("typeUSR", typeUSR(CA->getType()));
            attributeArray("args", CA->getArgs());
          });
        }
      });
    }
  }

  void visitValueDecl(ValueDecl *D) {
    visitDecl(D);
    attributeBool("isSynthesized", D->isSynthesized());
    if (D->hasInterfaceType()) {
      attributeString("interfaceTypeUSR", typeUSR(D->getInterfaceType()));
    }
    if (D->hasAccess()) {
      attributeString("access", getAccessLevelSpelling(D->getFormalAccess()));
    }
    attributeArray("overridenDeclUSRs", declUSRs(D->getOverriddenDecls()));
    attributeArray("satisfiedProtocolRequirementUSRs",
                   declUSRs(D->getSatisfiedProtocolRequirements()));
  }

  void visitTypeDecl(TypeDecl *D) {
    visitValueDecl(D);
    // Will be empty for OpaqueTypeDecls.
    if (!D->getName().empty()) {
      attribute("name", D->getName());
    }
    attributeString("declaredInterfaceTypeUSR",
                    typeUSR(D->getDeclaredInterfaceType()));
    attributeArray("inherited", D->getInherited().getEntries());
  }

  void visitGenericTypeDecl(GenericTypeDecl *D) {
    visitTypeDecl(D);
    dumpGenericContextCommon(D);
  }

  void dumpGenericContextCommon(GenericContext *G) {
    // We render generic signatures somewhat uniquely so that we can tie all
    // the available information together. We're interested in the following
    // information:
    //
    // 1.  The name of the parameter as given by the user.
    // 2.  It's declaration USR, which may appear in other declaration name
    //     references.
    // 3.  It's type USR, which only carries the index/depth information
    //     but is how the type appears in type references (like the
    //     requirements and in later substitution maps).
    //
    // If we iterate over the parameters in the GenericSignature, we only
    // get 3. The GenericContext has all there (the type USR is buried in
    // the parameter's `getDeclaredInterfaceType`). So, we print the
    // parameters from the context but the requirements from the signature.
    GenericSignature sig = G->getGenericSignature();
    if (!G->getGenericParams() && sig.getRequirements().empty()) {
      return;
    }
    json.attributeObject("genericSignature", [&] {
      if (auto params = G->getGenericParams()) {
        attributeArray("parameters", *params);
      }
      if (!sig.getRequirements().empty()) {
        json.attributeArray("requirements", [&] {
          for (auto req : sig.getRequirements()) {
            value(req);
          }
        });
      }
    });
  }

  void visitNominalTypeDecl(NominalTypeDecl *D) {
    visitGenericTypeDecl(D);
    attributeArray("members", D->getABIMembers());
  }

  void visitEnumDecl(EnumDecl *D) {
    visitNominalTypeDecl(D);
    attributeBool("isIndirect", D->isIndirect());
  }

  void visitStructDecl(StructDecl *D) {
    visitNominalTypeDecl(D);
    attributeBool("hasUnreferenceableStorage", D->hasUnreferenceableStorage());
    attributeBool("isCxxNonTrivial", D->isCxxNonTrivial());
    attributeBool("isNonTrivialPtrAuth", D->isNonTrivialPtrAuth());
  }

  void visitClassDecl(ClassDecl *D) {
    visitNominalTypeDecl(D);
    attributeBool("isExplicitDistributedActor",
                  D->isExplicitDistributedActor());
    attributeBool("isExplicitActor", D->isExplicitActor());
  }

  void visitProtocolDecl(ProtocolDecl *D) {
    visitNominalTypeDecl(D);
    attribute("requirementSignature", D->getRequirementSignature());
  }

  void visitBuiltinTupleDecl(BuiltinTupleDecl *D) { visitNominalTypeDecl(D); }

  void visitOpaqueTypeDecl(OpaqueTypeDecl *D) {
    visitGenericTypeDecl(D);
    attributeString("namingDeclUSR", declUSR(D->getNamingDecl()));
    attribute("uniqueUnderlyingTypeSubstitutions",
              D->getUniqueUnderlyingTypeSubstitutions());
  }

  void visitTypeAliasDecl(TypeAliasDecl *D) {
    visitGenericTypeDecl(D);
    attributeString("underlyingTypeUSR", typeUSR(D->getUnderlyingType()));
  }

  void visitGenericTypeParamDecl(GenericTypeParamDecl *D) {
    visitTypeDecl(D);
    attribute("index", D->getIndex());
    attribute("depth", D->getDepth());
    attributeString("paramKind",
                    jsonStringForGenericTypeParamKind(D->getParamKind()));
    attributeBool("isOpaqueType", D->isOpaqueType());
    attributeString("valueTypeUSR", typeUSR(D->getValueType()));
  }

  void visitAssociatedTypeDecl(AssociatedTypeDecl *D) {
    visitTypeDecl(D);
    attributeString("defaultTypeUSR", typeUSR(D->getDefaultDefinitionType()));
  }

  void visitModuleDecl(ModuleDecl *D) {
    attributeBool("isSystem", D->isSystemModule());
    attributeBool("isNonSwift", D->isNonSwiftModule());
    attributeBool("hasUnderlyingClangModule",
                  D->findUnderlyingClangModule() != nullptr);
  }

  void visitVarDecl(VarDecl *D) {
    visitAbstractStorageDecl(D);
    attribute("name", D->getName());
    attributeString("introducer", D->getIntroducerStringRef());
    attributeArray("accessors", D->getAllAccessors());
  }

  void visitParamDecl(ParamDecl *D) {
    visitVarDecl(D);
    attribute("label", D->getArgumentName());

    attributeString("specifier",
                    ParamDecl::getSpecifierSpelling(D->getSpecifier()));
    if (D->hasInterfaceType()) {
      attributeBool("isVariadic", D->isVariadic());
    }
    attributeBool("isAutoclosure", D->isAutoClosure());
    attribute("defaultExpr", D->getTypeCheckedDefaultExpr());
  }

  void visitAbstractStorageDecl(AbstractStorageDecl *D) {
    visitValueDecl(D);
    attributeString("staticSpelling", jsonStringForStaticSpellingKind(
                                          D->getCorrectStaticSpelling()));
    attributeString("overriddenDeclUSR", declUSR(D->getOverriddenDecl()));

    auto impl = D->getImplInfo();
    attributeString("readImpl", jsonStringForReadImplKind(impl.getReadImpl()));
    if (bool isMutable = impl.supportsMutation()) {
      attributeBool("supportsMutation", isMutable);
      attributeString("writeImpl",
                      jsonStringForWriteImplKind(impl.getWriteImpl()));
      attributeString("readWriteImpl",
                      jsonStringForReadWriteImplKind(impl.getReadWriteImpl()));
    }
  }

  void visitAbstractFunctionDecl(AbstractFunctionDecl *D) {
    visitValueDecl(D);
    attribute("name", D->getName());
    dumpGenericContextCommon(D);
    if (auto params = D->getParameters()) {
      attributeArray("parameters", *params);
    }
    attributeBool("hasThrows", D->hasThrows());
    attributeString("thrownTypeUSR", typeUSR(D->getThrownInterfaceType()));
    attributeBool("hasAsync", D->hasAsync());
    attributeString("implicitSelfUSR", declUSR(D->getImplicitSelfDecl()));
    attributeString("overriddenDeclUSR", declUSR(D->getOverriddenDecl()));
    attribute("opaqueResultType", D->getOpaqueResultTypeDecl());
    attributeBool("isDistributed", D->isDistributed());
    attributeBool("isDistributedThunk", D->isDistributedThunk());

    CaptureInfo captures = D->getCaptureInfo();
    if (captures.hasBeenComputed() && !captures.isTrivial()) {
      attribute("captures", captures);
    }

    // Don't dump the bodies of synthesized functions/accessors.
    if (!D->isSynthesized()) {
      attribute("body", D->getBody());
    }
  }

  void visitSubscriptDecl(SubscriptDecl *D) {
    visitAbstractStorageDecl(D);
    attribute("name", D->getName());
    attributeString("elementTypeUSR", typeUSR(D->getElementInterfaceType()));
    if (auto indices = D->getIndices()) {
      attributeArray("indices", *indices);
    }
    attributeArray("accessors", D->getAllAccessors());
  }

  void visitConstructorDecl(ConstructorDecl *D) {
    visitAbstractFunctionDecl(D);
    attributeBool("isConvenienceInit", D->isConvenienceInit());
    attributeBool("isRequired", D->isRequired());
    if (D->isFailable()) {
      attributeString("failable", D->isImplicitlyUnwrappedOptional()
                                      ? "implicitlyUnwrappedOptional"
                                      : "optional");
    }
    attributeString("initKind",
                    jsonStringForCtorInitializerKind(D->getInitKind()));
  }

  void visitDestructorDecl(DestructorDecl *D) { visitAbstractFunctionDecl(D); }

  void visitFuncDecl(FuncDecl *D) {
    visitAbstractFunctionDecl(D);
    attributeString("staticSpelling",
                    jsonStringForStaticSpellingKind(D->getStaticSpelling()));
    attributeString("resultTypeUSR", typeUSR(D->getResultInterfaceType()));
  }

  void visitAccessorDecl(AccessorDecl *D) {
    visitFuncDecl(D);
    attributeString("accessorKind",
                    jsonStringForAccessorKind(D->getAccessorKind()));
    attributeString("storage", declUSR(D->getStorage()));
    if (D->isInitAccessor()) {
      std::vector<std::string> initUSRs =
          declUSRs(D->getInitializedProperties());
      if (!initUSRs.empty()) {
        attributeArray("initializes", initUSRs);
      }
      std::vector<std::string> accessUSRs =
          declUSRs(D->getAccessedProperties());
      if (!accessUSRs.empty()) {
        attributeArray("accesses", accessUSRs);
      }
    }
  }

  void visitMacroDecl(MacroDecl *D) {
    visitValueDecl(D);
    attribute("name", D->getName());
    attribute("definition", D->definition);
    attributeString("resultTypeUSR", typeUSR(D->getResultInterfaceType()));
  }

  void visitExtensionDecl(ExtensionDecl *D) {
    visitDecl(D);
    if (D->hasBeenBound()) {
      attributeString("extendedNominalDeclUSR",
                      declUSR(D->getExtendedNominal()));
    }
    attributeString("extendedTypeUSR", typeUSR(D->getExtendedType()));
    attributeArray("inherited", D->getInherited().getEntries());
    attributeArray("members", D->getABIMembers());
  }

  void visitTopLevelCodeDecl(TopLevelCodeDecl *D) {
    visitDecl(D);
    attribute("body", D->getBody());
  }

  void visitImportDecl(ImportDecl *D) {
    visitDecl(D);
    attributeString("importKind", jsonStringForImportKind(D->getImportKind()));
    attributeString("access", getAccessLevelSpelling(D->getAccessLevel()));
    attributeBool("isAccessImplicit", D->isAccessLevelImplicit());
    attributeBool("isExported", D->isExported());
    attributeBool("isTestable", D->isTestable());
    attributeArray("modulePath", D->getModulePath());
    attributeArray("accessPath", D->getAccessPath());
    attribute("moduleInfo", D->getModule());
  }

  void visitPoundDiagnosticDecl(PoundDiagnosticDecl *D) {
    visitDecl(D);
    attributeBool("isError", D->isError());
    attribute("message", D->getMessage());
  }

  void visitPrecedenceGroupDecl(PrecedenceGroupDecl *D) {
    visitDecl(D);
    attribute("name", D->getName());
    attributeString("associativity",
                    jsonStringForAssociativity(D->getAssociativity()));
    attributeBool("isAssignment", D->isAssignment());
    attributeArray("higherThan", D->getHigherThan());
    attributeArray("lowerThan", D->getLowerThan());
  }

  void visitMissingDecl(MissingDecl *D) {}

  void visitMissingMemberDecl(MissingMemberDecl *D) {}

  void visitPatternBindingDecl(PatternBindingDecl *D) {
    visitDecl(D);
    attributeBool("isAsyncLet", D->isAsyncLet());
    attributeString("staticSpelling", jsonStringForStaticSpellingKind(
                                          D->getCorrectStaticSpelling()));
    if (D->getNumPatternEntries() != 0) {
      json.attributeArray("entries", [&] {
        for (size_t i = 0; i < D->getNumPatternEntries(); ++i) {
          json.object([&] {
            attribute("pattern", D->getPattern(i));
            attribute("processedInit", D->getInit(i));
            // originalInit isn't interesting enough to take up as much space
            // as it would in the output. processedInit has the folded, fully
            // type checked expression.
            CaptureInfo captures = D->getCaptureInfo(i);
            if (captures.hasBeenComputed() && !captures.isTrivial()) {
              attribute("captures", captures);
            }
          });
        }
      });
    }
  }

  void visitEnumCaseDecl(EnumCaseDecl *D) {
    visitDecl(D);

    // The EnumElements will also be written under the "members" key. To
    // avoid redundant information, we just write the elements' USRs here.
    // This allows users to relate which members were grouped together in
    // which `case` declarations if they wish.
    json.attributeBegin("elements");
    json.arrayBegin();
    for (const auto *element : D->getElements()) {
      json.value(declUSR(element));
    }
    json.arrayEnd();
    json.attributeEnd();
  }

  void visitEnumElementDecl(EnumElementDecl *D) {
    visitValueDecl(D);
    attributeBool("isIndirect", D->isIndirect());
    if (auto params = D->getParameterList()) {
      attributeArray("parameters", *params);
    }
  }

  void visitOperatorDecl(OperatorDecl *D) {
    visitDecl(D);
    attribute("name", D->getName());
  }

  void visitInfixOperatorDecl(InfixOperatorDecl *D) {
    visitOperatorDecl(D);
    attributeString("precedenceGroup", D->getPrecedenceGroupName().str());
  }

  void visitPrefixOperatorDecl(PrefixOperatorDecl *D) { visitOperatorDecl(D); }

  void visitPostfixOperatorDecl(PostfixOperatorDecl *D) {
    visitOperatorDecl(D);
  }

  void visitMacroExpansionDecl(MacroExpansionDecl *D) {
    visitDecl(D);
    attribute("macroRef", D->getMacroRef());
    attribute("discriminator", D->getRawDiscriminator());
    attributeArray("genericArgs", D->getGenericArgs());
    attributeArray("args", D->getArgs());

    json.attributeArray("expansion", [&] {
      D->forEachExpandedNode([&](ASTNode node) { value(node); });
    });
  }

  // MARK: Statements

  void visitBraceStmt(BraceStmt *S) {
    attributeArray("elements", S->getElements());
  }

  void visitReturnStmt(ReturnStmt *S) {
    if (S->hasResult()) {
      attribute("result", S->getResult());
    }
  }

  void visitThenStmt(ThenStmt *S) { attribute("result", S->getResult()); }

  void visitYieldStmt(YieldStmt *S) {
    attributeArray("yields", S->getYields());
  }

  void visitDeferStmt(DeferStmt *S) {
    attribute("tempDecl", S->getTempDecl());
    attribute("tempCallExpr", S->getCallExpr());
  }

  void visitIfStmt(IfStmt *S) {
    visitLabeledConditionalStmt(S);
    attribute("then", S->getThenStmt());
    attribute("else", S->getElseStmt());
  }

  void visitGuardStmt(GuardStmt *S) {
    visitLabeledConditionalStmt(S);
    attribute("body", S->getBody());
  }

  void visitWhileStmt(WhileStmt *S) {
    visitLabeledConditionalStmt(S);
    attribute("body", S->getBody());
  }

  void visitDoStmt(DoStmt *S) {
    visitLabeledStmt(S);
    attribute("body", S->getBody());
  }

  void visitDoCatchStmt(DoCatchStmt *S) {
    visitLabeledStmt(S);
    attributeBool("isSyntacticallyExhaustive", S->isSyntacticallyExhaustive());
    attribute("caughtErrorTypeUSR", typeUSR(S->getCaughtErrorType()));
    attribute("body", S->getBody());
    attributeArray("catches", S->getCatches());
  }

  void visitRepeatWhileStmt(RepeatWhileStmt *S) {
    attribute("condition", S->getCond());
    attribute("body", S->getBody());
  }

  void visitForEachStmt(ForEachStmt *S) {
    attribute("pattern", S->getPattern());
    attribute("sequenceExpr", S->getTypeCheckedSequence());
    attribute("sequenceTypeUSR", typeUSR(S->getSequenceType()));
    attribute("sequenceConformance", S->getSequenceConformance());
    attribute("whereExpr", S->getWhere());
    attribute("body", S->getBody());
  }

  void visitSwitchStmt(SwitchStmt *S) {
    attribute("subjectExpr", S->getSubjectExpr());
    attributeArray("cases", S->getRawCases());
  }

  void visitCaseStmt(CaseStmt *S) {
    attributeBool("hasBoundDecls", S->hasBoundDecls());
    attributeBool("hasUnknownAttr", S->hasUnknownAttr());
    attributeBool("isDefault", S->isDefault());
    attributeArray("caseLabelItems", S->getCaseLabelItems());
    attribute("body", S->getBody());
  }

  void visitBreakStmt(BreakStmt *S) {
    attributeString("target", S->getTargetName().str());
  }

  void visitContinueStmt(ContinueStmt *S) {
    attributeString("target", S->getTargetName().str());
  }

  void visitFallthroughStmt(FallthroughStmt *S) {}

  void visitFailStmt(FailStmt *S) {}

  void visitThrowStmt(ThrowStmt *S) { attribute("subExpr", S->getSubExpr()); }

  void visitDiscardStmt(DiscardStmt *S) {
    attribute("subExpr", S->getSubExpr());
  }

  void visitPoundAssertStmt(PoundAssertStmt *S) {
    attribute("condition", S->getCondition());
    attributeString("message", S->getMessage());
  }

  void visitLabeledStmt(LabeledStmt *S) {
    if (!S->getLabelInfo().Name.empty()) {
      attribute("label", S->getLabelInfo().Name);
    }
  }

  void visitLabeledConditionalStmt(LabeledConditionalStmt *S) {
    visitLabeledStmt(S);
    attributeArray("conditions", S->getCond());
  }

  // MARK: Expressions

  void visitExpr(Expr *E) { attribute("typeUSR", typeUSR(E->getType())); }

  void visitLiteralExpr(LiteralExpr *E) {
    visitExpr(E);
    attribute("initializer", E->getInitializer());
  }

  void visitBuiltinLiteralExpr(BuiltinLiteralExpr *E) {
    visitLiteralExpr(E);
    attribute("builtinInitializer", E->getBuiltinInitializer());
  }

  void visitBooleanLiteralExpr(BooleanLiteralExpr *E) {
    visitBuiltinLiteralExpr(E);
    attributeBool("value", E->getValue());
  }

  void visitNumberLiteralExpr(NumberLiteralExpr *E) {
    visitBuiltinLiteralExpr(E);
    attributeString("digits", E->getDigitsText());
    attributeBool("isExplicitConversion", E->isExplicitConversion());
    attributeBool("isNegative", E->isNegative());
  }

  void visitFloatLiteralExpr(FloatLiteralExpr *E) {
    visitNumberLiteralExpr(E);
    attributeString("builtinType", typeUSR(E->getBuiltinType()));
  }

  void visitStringLiteralExpr(StringLiteralExpr *E) {
    visitBuiltinLiteralExpr(E);
    attributeString("value", E->getValue());
    attributeBool("isSingleExtendedGraphemeCluster",
                  E->isSingleExtendedGraphemeCluster());
    attributeBool("isSingleUnicodeScalar", E->isSingleUnicodeScalar());
  }

  void visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *E) {
    visitBuiltinLiteralExpr(E);
    attributeString("magicIdentifierKind",
                    MagicIdentifierLiteralExpr::getKindString(E->getKind()));
  }

  void visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *E) {
    visitLiteralExpr(E);
    attribute("builderInit", E->getBuilderInit());
    attribute("interpolationCount", E->getInterpolationCount());
    attribute("literalCapacity", E->getLiteralCapacity());
    attribute("appendingExpr", E->getAppendingExpr());
  }

  void visitRegexLiteralExpr(RegexLiteralExpr *E) {
    visitLiteralExpr(E);
    attributeString("regex", E->getRegexToEmit());
    attributeString("regexTypeUSR", typeUSR(E->getRegexType()));
    attribute("version", E->getVersion());
  }

  void visitObjectLiteralExpr(ObjectLiteralExpr *E) {
    visitLiteralExpr(E);
    attributeString("literalKind", E->getLiteralKindPlainName());
    attributeArray("args", E->getArgs());
  }

  void visitDeclRefExpr(DeclRefExpr *E) {
    visitExpr(E);
    attribute("declRef", E->getDeclRef());
  }

  void visitSuperRefExpr(SuperRefExpr *E) {
    visitExpr(E);
    attributeString("selfUSR", declUSR(E->getSelf()));
  }

  void visitTypeExpr(TypeExpr *E) {
    visitExpr(E);
    attributeString("instanceTypeUSR", typeUSR(E->getInstanceType()));
  }

  void visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *E) {
    visitExpr(E);
    attribute("declRef", E->getDeclRef());
  }

  void visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
    visitExpr(E);
    attribute("lhs", E->getLHS());
    attribute("rhs", E->getRHS());
  }

  void visitOverloadSetRefExpr(OverloadSetRefExpr *E) {
    visitExpr(E);
    attributeArray("declUSRs", declUSRs(E->getDecls()));
  }

  void visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) {
    visitExpr(E);
    attribute("name", E->getName());
  }

  void visitLookupExpr(LookupExpr *E) {
    visitExpr(E);
    attribute("base", E->getBase());
    attribute("member", E->getMember());
  }

  void visitSubscriptExpr(SubscriptExpr *E) {
    visitLookupExpr(E);
    attributeArray("args", E->getArgs());
  }

  void visitDynamicSubscriptExpr(DynamicSubscriptExpr *E) {
    visitLookupExpr(E);
    attributeArray("args", E->getArgs());
  }

  void visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *E) {
    // Should not appear in the type-checked AST.
  }

  void visitUnresolvedMemberExpr(UnresolvedMemberExpr *E) {
    visitExpr(E);
    attribute("name", E->getName());
  }

  void visitUnresolvedDotExpr(UnresolvedDotExpr *E) {
    visitExpr(E);
    attribute("base", E->getBase());
    attribute("name", E->getName());
  }

  void visitSequenceExpr(SequenceExpr *E) {
    visitExpr(E);
    attributeArray("elements", E->getElements());
  }

  void visitIdentityExpr(IdentityExpr *E) {
    visitExpr(E);
    attribute("subExpr", E->getSubExpr());
  }

  void visitCopyExpr(CopyExpr *E) {
    visitExpr(E);
    attribute("subExpr", E->getSubExpr());
  }

  void visitConsumeExpr(ConsumeExpr *E) {
    visitExpr(E);
    attribute("subExpr", E->getSubExpr());
  }

  void visitAnyTryExpr(AnyTryExpr *E) {
    visitExpr(E);
    attribute("subExpr", E->getSubExpr());
  }

  void visitForceTryExpr(ForceTryExpr *E) {
    visitAnyTryExpr(E);
    attributeString("throwErrorTypeUSR", typeUSR(E->getThrownError()));
  }

  void visitOptionalTryExpr(OptionalTryExpr *E) {
    visitAnyTryExpr(E);
    attributeString("throwErrorTypeUSR", typeUSR(E->getThrownError()));
  }

  void visitTupleExpr(TupleExpr *E) {
    visitExpr(E);
    attributeArray("elements", E->getElements());
  }

  void visitCollectionExpr(CollectionExpr *E) {
    visitExpr(E);
    attribute("initializer", E->getInitializer());
    attributeBool("isTypeDefaulted", E->isTypeDefaulted());
  }

  void visitArrayExpr(ArrayExpr *E) {
    visitCollectionExpr(E);
    attributeString("elementTypeUSR", typeUSR(E->getElementType()));
    attributeArray("elements", E->getElements());
  }

  void visitDictionaryExpr(DictionaryExpr *E) {
    visitCollectionExpr(E);
    attributeString("elementTypeUSR", typeUSR(E->getElementType()));
    attributeArray("elements", E->getElements());
  }

  void visitKeyPathApplicationExpr(KeyPathApplicationExpr *E) {
    visitExpr(E);
    attribute("base", E->getBase());
    attribute("keyPath", E->getKeyPath());
  }

  void visitTupleElementExpr(TupleElementExpr *E) {
    visitExpr(E);
    attribute("base", E->getBase());
    attribute("fieldNumber", E->getFieldNumber());
  }

  void visitCaptureListExpr(CaptureListExpr *E) {
    visitExpr(E);
    attributeArray("captureList", E->getCaptureList());
    attribute("body", E->getClosureBody());
  }

  void visitAbstractClosureExpr(AbstractClosureExpr *E) {
    visitExpr(E);

    CaptureInfo captures = E->getCaptureInfo();
    if (captures.hasBeenComputed() && !captures.isTrivial()) {
      attribute("captures", captures);
    }
    attribute("actorIsolation", E->getActorIsolation());
    attributeArray("parameters", *E->getParameters());
    attribute("resultTypeUSR", typeUSR(E->getResultType()));
    if (auto error = E->getEffectiveThrownType()) {
      attribute("effectiveThrownTypeUSR", typeUSR(*error));
    }
    attributeBool("isBodyThrowing", E->isBodyThrowing());
    attributeBool("isBodyAsync", E->isBodyAsync());
    attributeBool("isSendable", E->isSendable());
    attributeBool("hasSingleExpressionBody", E->hasSingleExpressionBody());
    attribute("body", E->getBody());
  }

  void visitClosureExpr(ClosureExpr *E) {
    visitAbstractClosureExpr(E);
    json.attributeArray("attributes", [&] {
      for (auto A : E->getAttrs()) {
        writeAttr(A, E);
      }
    });
    attributeString("explicitThrownTypeUSR",
                    typeUSR(E->getExplicitThrownType()));
    attributeBool("allowsImplicitSelfCapture", E->allowsImplicitSelfCapture());
    attributeBool("hasAnonymousClosureVars", E->hasAnonymousClosureVars());
    attributeBool("inheritsActorContext", E->inheritsActorContext());
    attributeBool("isIsolatedByPreconcurrency",
                  E->isIsolatedByPreconcurrency());
    attributeBool("isPassedToSendingParameter",
                  E->isPassedToSendingParameter());
    attributeBool("requiresDynamicIsolationChecking",
                  E->requiresDynamicIsolationChecking());
  }

  void visitAutoClosureExpr(AutoClosureExpr *E) {
    visitAbstractClosureExpr(E);
    attribute("body", E->getBody());
  }

  void visitInOutExpr(InOutExpr *E) {
    visitExpr(E);
    attribute("subExpr", E->getSubExpr());
  }

  void visitVarargExpansionExpr(VarargExpansionExpr *E) {
    visitExpr(E);
    attribute("subExpr", E->getSubExpr());
  }

  void visitPackExpansionExpr(PackExpansionExpr *E) {
    visitExpr(E);
    attribute("patternExpr", E->getPatternExpr());
  }

  void visitPackElementExpr(PackElementExpr *E) {
    visitExpr(E);
    attribute("packRefExpr", E->getPackRefExpr());
  }

  void visitMaterializePackExpr(MaterializePackExpr *E) {
    visitExpr(E);
    attribute("fromExpr", E->getFromExpr());
  }

  void visitExtractFunctionIsolationExpr(ExtractFunctionIsolationExpr *E) {
    visitExpr(E);
    attribute("fnExpr", E->getFunctionExpr());
  }

  void visitDynamicTypeExpr(DynamicTypeExpr *E) {
    visitExpr(E);
    attribute("base", E->getBase());
  }

  void visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E) {
    visitExpr(E);
    attributeString("selfDeclUSR", declUSR(E->getSelf()));
    attribute("subExpr", E->getSubExpr());
  }

  void visitOpaqueValueExpr(OpaqueValueExpr *E) {
    visitExpr(E);
    attribute("isPlaceholder", E->isPlaceholder());
  }

  void visitPropertyWrapperValuePlaceholderExpr(
      PropertyWrapperValuePlaceholderExpr *E) {
    visitExpr(E);
    attribute("wrappedValue", E->getOriginalWrappedValue());
    attribute("opaqueValuePlaceholder", E->getOpaqueValuePlaceholder());
    attributeBool("isAutoClosure", E->isAutoClosure());
  }

  void visitAppliedPropertyWrapperExpr(AppliedPropertyWrapperExpr *E) {
    visitExpr(E);
    attribute("callee", E->getCallee());
    attribute("paramDeclUSR", declUSR(E->getParamDecl()));
    attributeString("valueKind",
                    jsonStringForPropertyWrapperValueKind(E->getValueKind()));
    attribute("value", E->getValue());
  }

  void visitDefaultArgumentExpr(DefaultArgumentExpr *E) {
    visitExpr(E);
    attribute("owner", E->getDefaultArgsOwner());
    attribute("paramIndex", E->getParamIndex());
    attribute("paramDeclUSR", declUSR(E->getParamDecl()));
    if (E->isCallerSide()) {
      attributeBool("isCallerSide", E->isCallerSide());
      attribute("callerSideDefaultExpr", E->getCallerSideDefaultExpr());
    }
    attributeBool("isImplicitlyAsync", E->isImplicitlyAsync());
    attribute("requiredIsolation", E->getRequiredIsolation());
  }

  void visitBindOptionalExpr(BindOptionalExpr *E) {
    visitExpr(E);
    attribute("depth", E->getDepth());
    attribute("subExpr", E->getSubExpr());
  }

  void visitOptionalEvaluationExpr(OptionalEvaluationExpr *E) {
    visitExpr(E);
    attribute("subExpr", E->getSubExpr());
  }

  void visitForceValueExpr(ForceValueExpr *E) {
    visitExpr(E);
    attributeBool("isForceOfIUO", E->isForceOfImplicitlyUnwrappedOptional());
    attribute("subExpr", E->getSubExpr());
  }

  void visitOpenExistentialExpr(OpenExistentialExpr *E) {
    visitExpr(E);
    attribute("subExpr", E->getSubExpr());
  }

  void visitMakeTemporarilyEscapableExpr(MakeTemporarilyEscapableExpr *E) {
    visitExpr(E);
    attribute("subExpr", E->getSubExpr());
  }

  void visitApplyExpr(ApplyExpr *E) {
    visitExpr(E);
    attribute("fn", E->getFn());
    attributeArray("args", E->getArgs());
    attribute("isolationCrossing", E->getIsolationCrossing());
    if (E->isThrowsSet()) {
      attribute("throws", E->throws());
      attributeBool("noThrows", E->isNoThrows());
    }
    attributeBool("noAsync", E->isNoAsync());
  }

  void visitImplicitConversionExpr(ImplicitConversionExpr *E) {
    visitExpr(E);
    attribute("subExpr", E->getSubExpr());
  }

  void visitDestructureTupleExpr(DestructureTupleExpr *E) {
    visitImplicitConversionExpr(E);
    attribute("resultExpr", E->getResultExpr());
    attributeArray("destructured", E->getDestructuredElements());
  }

  void visitCollectionUpcastConversionExpr(CollectionUpcastConversionExpr *E) {
    visitImplicitConversionExpr(E);
    attribute("keyConversion", E->getKeyConversion());
    attribute("valueConversion", E->getValueConversion());
  }

  void visitErasureExpr(ErasureExpr *E) {
    visitImplicitConversionExpr(E);
    attributeArray("conformances", E->getConformances());
    attributeArray("conversions", E->getArgumentConversions());
  }

  void visitAnyHashableErasureExpr(AnyHashableErasureExpr *E) {
    visitImplicitConversionExpr(E);
    attribute("conformance", E->getConformance());
  }

  void visitConditionalBridgeFromObjCExpr(ConditionalBridgeFromObjCExpr *E) {
    visitImplicitConversionExpr(E);
    attribute("conversion", E->getConversion());
  }

  void visitInOutToPointerExpr(InOutToPointerExpr *E) {
    visitImplicitConversionExpr(E);
    attributeBool("isNonAccessing", E->isNonAccessing());
  }

  void visitArrayToPointerExpr(ArrayToPointerExpr *E) {
    visitImplicitConversionExpr(E);
    attributeBool("isNonAccessing", E->isNonAccessing());
  }

  void visitUnderlyingToOpaqueExpr(UnderlyingToOpaqueExpr *E) {
    visitImplicitConversionExpr(E);
    attribute("substitutions", E->substitutions);
  }

  void visitExplicitCastExpr(ExplicitCastExpr *E) {
    visitExpr(E);
    attribute("castTypeUSR", typeUSR(E->getCastType()));
    attribute("subExpr", E->getSubExpr());
  }

  void visitCheckedCastExpr(CheckedCastExpr *E) {
    visitExplicitCastExpr(E);
    attributeString("castKind", getCheckedCastKindName(E->getCastKind()));
  }

  void visitArrowExpr(ArrowExpr *E) {
    visitExpr(E);
    // Not interesting; does not appear in type-checked ASTs.
  }

  void visitTernaryExpr(TernaryExpr *E) {
    visitExpr(E);
    attribute("condition", E->getCondExpr());
    attribute("then", E->getThenExpr());
    attribute("else", E->getElseExpr());
  }

  void visitEnumIsCaseExpr(EnumIsCaseExpr *E) {
    visitExpr(E);
    attribute("subExpr", E->getSubExpr());
    attributeString("enumElementDeclUSR", declUSR(E->getEnumElement()));
  }

  void visitAssignExpr(AssignExpr *E) {
    visitExpr(E);
    attribute("dest", E->getDest());
    attribute("src", E->getSrc());
  }

  void visitCodeCompletionExpr(CodeCompletionExpr *E) {
    visitExpr(E);
    attribute("base", E->getBase());
  }

  void visitUnresolvedPatternExpr(UnresolvedPatternExpr *E) {
    visitExpr(E);
    attribute("subPattern", E->getSubPattern());
  }

  void visitLazyInitializerExpr(LazyInitializerExpr *E) {
    visitExpr(E);
    attribute("subExpr", E->getSubExpr());
  }

  void visitEditorPlaceholderExpr(EditorPlaceholderExpr *E) {
    visitExpr(E);
    attribute("placeholder", E->getPlaceholder());
  }

  void visitObjCSelectorExpr(ObjCSelectorExpr *E) {
    visitExpr(E);
    attribute("subExpr", E->getSubExpr());
    attribute("methodDeclUSR", declUSR(E->getMethod()));
  }

  void visitKeyPathExpr(KeyPathExpr *E) {
    visitExpr(E);
    attribute("objcStringLiteral", E->getObjCStringLiteralExpr());
    attributeArray("components", E->getComponents());
  }

  void visitCurrentContextIsolationExpr(CurrentContextIsolationExpr *E) {
    visitExpr(E);
    attribute("actor", E->getActor());
  }

  void visitSingleValueStmtExpr(SingleValueStmtExpr *E) {
    visitExpr(E);
    attribute("stmt", E->getStmt());
  }

  void visitOneWayExpr(OneWayExpr *E) {
    visitExpr(E);
    attribute("subExpr", E->getSubExpr());
  }

  void visitTapExpr(TapExpr *E) {
    visitExpr(E);
    attribute("subExpr", E->getSubExpr());
    attribute("body", E->getBody());
  }

  void visitTypeJoinExpr(TypeJoinExpr *E) {
    visitExpr(E);
    attribute("var", E->getVar());
    attribute("singleValueStmtExpr", E->getSingleValueStmtExpr());
    attributeArray("elements", E->getElements());
  }

  void visitMacroExpansionExpr(MacroExpansionExpr *E) {
    visitExpr(E);
    attribute("macroRef", E->getMacroRef());
    attribute("discriminator", E->getRawDiscriminator());
    attributeArray("args", E->getArgs());
    attribute("rewritten", E->getRewritten());
  }

  void visitTypeValueExpr(TypeValueExpr *E) {
    visitExpr(E);
    attributeString("paramTypeUSR", typeUSR(Type(E->getParamType())));
  }

  // MARK: Patterns

  void visitPattern(Pattern *P) {
    if (P->hasType()) {
      attribute("typeUSR", typeUSR(P->getType()));
    }
  }

  void visitParenPattern(ParenPattern *P) {
    visitPattern(P);
    attribute("subPattern", P->getSubPattern());
  }

  void visitTuplePattern(TuplePattern *P) {
    visitPattern(P);
    attributeArray("elements", P->getElements());
  }

  void visitNamedPattern(NamedPattern *P) {
    visitPattern(P);
    attributeString("declUSR", declUSR(P->getDecl()));
  }

  void visitAnyPattern(AnyPattern *P) {
    visitPattern(P);
    attributeBool("isAsyncLet", P->isAsyncLet());
  }

  void visitTypedPattern(TypedPattern *P) {
    visitPattern(P);
    attribute("subPattern", P->getSubPattern());
    attributeString("typeUSR", typeUSR(P->getType()));
  }

  void visitBindingPattern(BindingPattern *P) {
    visitPattern(P);
    attributeString("introducer", P->getIntroducerStringRef());
    attribute("subPattern", P->getSubPattern());
  }

  void visitIsPattern(IsPattern *P) {
    visitPattern(P);
    attribute("subPattern", P->getSubPattern());
    attributeString("castKind", getCheckedCastKindName(P->getCastKind()));
    attributeString("castTypeUSR", typeUSR(P->getCastType()));
  }

  void visitEnumElementPattern(EnumElementPattern *P) {
    visitPattern(P);
    attribute("name", P->getName());
    attributeString("parentTypeUSR", typeUSR(P->getParentType()));
    if (P->hasUnresolvedOriginalExpr()) {
      attribute("unresolvedExpr", P->getUnresolvedOriginalExpr());
    } else {
      attributeString("elementDeclUSR", declUSR(P->getElementDecl()));
    }
    attribute("subPattern", P->getSubPattern());
  }

  void visitOptionalSomePattern(OptionalSomePattern *P) {
    visitPattern(P);
    attribute("subPattern", P->getSubPattern());
  }

  void visitBoolPattern(BoolPattern *P) {
    visitPattern(P);
    attributeBool("value", P->getValue());
  }

  void visitExprPattern(ExprPattern *P) {
    visitPattern(P);
    if (auto E = P->getCachedMatchExpr()) {
      attribute("subExpr", E);
    } else {
      attribute("subExpr", P->getSubExpr());
    }
    attributeString("ownership",
                    getOwnershipSpelling(P->getCachedMatchOperandOwnership()));
  }

  // MARK: Types

  void visitTypeRepr(TypeRepr *T) {
    // We don't render TypeReprs. Use the syntactic AST if you're interested
    // in the precise spelling of types.
  }

private:
  // MARK: General writing helpers

  // `attribute` and `attributeArray` are generic functions that call the
  // matching `value` function on whatever argument they are passed.

  void value(StringRef s) { json.value(s); }

  void value(int s) { json.value(s); }

  void value(ASTNode node) {
    json.object([&] {
      writeKindAttribute(node);
      attributeBool("isImplicit", node.isImplicit());
      writeSourceRange(node);

      if (auto D = node.dyn_cast<Decl *>()) {
        visit(D);
      } else if (auto S = node.dyn_cast<Stmt *>()) {
        visit(S);
      } else if (auto P = node.dyn_cast<Pattern *>()) {
        visit(P);
      } else if (auto E = node.dyn_cast<Expr *>()) {
        visit(E);
      }
    });
  }

  void value(const ActorIsolation &isolation) {
    json.object([&] {
      switch (isolation) {
      case ActorIsolation::Unspecified:
        attribute("kind", "unspecified");
        break;
      case ActorIsolation::NonisolatedUnsafe:
        attribute("kind", "nonisolatedUnsafe");
        break;
      case ActorIsolation::Nonisolated:
        attribute("kind", "nonisolated");
        break;
      case ActorIsolation::Erased:
        attribute("kind", "erased");
        break;
      case ActorIsolation::ActorInstance:
        attribute("kind", "actorInstance");
        attribute("actorInstanceUSR", declUSR(isolation.getActorInstance()));
        break;
      case ActorIsolation::GlobalActor:
        attribute("kind", "globalActor");
        attribute("globalActorTypeUSR", typeUSR(isolation.getGlobalActor()));
        break;
      }
    });
  }

  void value(const ApplyIsolationCrossing &crossing) {
    json.object([&] {
      attribute("calleeIsolation", crossing.getCalleeIsolation());
      attribute("callerIsolation", crossing.getCallerIsolation());
    });
  }

  void value(Argument arg) {
    json.object([&] {
      attribute("label", arg.getLabel());
      attribute("expr", arg.getExpr());
    });
  }

  void value(const AvailabilitySpec *spec) {
    json.object([&] {
      attributeString("kind",
                      jsonStringForAvailabilitySpecKind(spec->getKind()));
      if (auto S = dyn_cast<PlatformVersionConstraintAvailabilitySpec>(spec)) {
        attributeString("platform", platformString(S->getPlatform()));
        attributeString("version", S->getVersion().getAsString());
      } else if (auto S = dyn_cast<
                     PlatformAgnosticVersionConstraintAvailabilitySpec>(spec)) {
        attributeString("version", S->getVersion().getAsString());
      } else if (auto S = dyn_cast<OtherPlatformAvailabilitySpec>(spec)) {
        // Nothing else to write.
      }
    });
  }

  void value(const CaptureInfo &captures) {
    json.object([&] { attributeArray("values", captures.getCaptures()); });
  }

  void value(const CaptureListEntry &entry) {
    json.object([&] { attribute("patternBinding", entry.PBD); });
  }

  void value(const CapturedValue &value) {
    json.object([&] {
      attributeBool("isDirect", value.isDirect());
      attributeBool("isNoEscape", value.isNoEscape());
      attributeBool("isLocalCapture", value.isLocalCapture());
      attribute("declUSR", declUSR(value.getDecl()));
      attribute("expr", value.getExpr());
    });
  }

  void value(CaseLabelItem item) {
    json.object([&] {
      attributeBool("isDefault", item.isDefault());
      attribute("pattern", item.getPattern());
      attribute("whereExpr", item.getGuardExpr());
    });
  }

  void value(CollectionUpcastConversionExpr::ConversionPair pair) {
    json.object([&] {
      attribute("conversion", pair.Conversion);
      attribute("originalValue", pair.OrigValue);
    });
  }

  // Written this way to match *only* `ConcreteDeclRef` and prevent overload
  // resolution from attempting to match other types via its constructors.
  template <typename T, typename std::enable_if<std::is_same<
                            T, ConcreteDeclRef>::value>::type * = nullptr>
  void value(const T &ref) {
    json.object([&] {
      if (auto decl = ref.getDecl()) {
        attribute("baseName", decl->getBaseName());
        attributeString("declUSR", declUSR(decl));
        attributeString("typeUSR", declTypeUSR(decl));
      }
      if (!ref.getSubstitutions().empty()) {
        attribute("substitutions", ref.getSubstitutions());
      }
    });
  }

  // Written this way to match *only* `DeclBaseName` and prevent overload
  // resolution from attempting to match other types via its constructors.
  template <typename T,
            typename std::enable_if<std::is_same<T, DeclBaseName>::value>::type
                * = nullptr>
  void value(const T &baseName) {
    json.object([&] {
      switch (baseName.getKind()) {
      case DeclBaseName::Kind::Constructor:
        attributeString("special", "init");
        break;
      case DeclBaseName::Kind::Destructor:
        attributeString("special", "deinit");
        break;
      case DeclBaseName::Kind::Subscript:
        attributeString("special", "subscript");
        break;
      case DeclBaseName::Kind::Normal:
        attribute("name", baseName.getIdentifier());
        attributeBool("isOperator", baseName.isOperator());
      }
    });
  }

  // Written this way to match *only* `DeclName` and prevent overload
  // resolution from attempting to match other types via its constructors.
  template <typename T, typename std::enable_if<
                            std::is_same<T, DeclName>::value>::type * = nullptr>
  void value(const T &declName) {
    json.object([&] {
      attribute("baseName", declName.getBaseName());
      attributeArray("arguments", declName.getArgumentNames());
    });
  }

  // Written this way to match *only* `DeclNameRef` and prevent overload
  // resolution from attempting to match other types via its constructors.
  template <typename T,
            typename std::enable_if<std::is_same<T, DeclNameRef>::value>::type
                * = nullptr>
  void value(const T &ref) {
    value(ref.getFullName());
  }

  // Written this way to match *only* `Identifier` and prevent overload
  // resolution from attempting to match other types via its constructors.
  template <typename T,
            typename std::enable_if<std::is_same<T, Identifier>::value>::type
                * = nullptr>
  void value(const T &ident) {
    json.value(ident.str());
  }

  void value(const InheritedEntry &entry) {
    json.object([&] {
      attributeString("typeUSR", typeUSR(entry.getType()));
      attributeBool("isPreconcurrency", entry.isPreconcurrency());
      attributeBool("isRetroactive", entry.isRetroactive());
      attributeBool("isSuppressed", entry.isSuppressed());
      attributeBool("isUnchecked", entry.isUnchecked());
    });
  }

  void value(KeyPathExpr::Component component) {
    json.object([&] {
      attributeString(
          "kind", jsonStringForKeyPathExprComponentKind(component.getKind()));
      attributeString("typeUSR", typeUSR(component.getComponentType()));
      switch (component.getKind()) {
      case KeyPathExpr::Component::Kind::UnresolvedProperty:
        attribute("unresolvedName", component.getUnresolvedDeclName());
        break;
      case KeyPathExpr::Component::Kind::UnresolvedSubscript:
        attributeArray("args", *component.getSubscriptArgs());
        break;
      case KeyPathExpr::Component::Kind::Property:
        attribute("resolvedDeclRef", component.getDeclRef());
        break;
      case KeyPathExpr::Component::Kind::Subscript:
        attribute("resolvedDeclRef", component.getDeclRef());
        attributeArray("args", *component.getSubscriptArgs());
        attributeArray("hashableConformances",
                       component.getSubscriptIndexHashableConformances());
        break;
      case KeyPathExpr::Component::Kind::TupleElement:
        attribute("index", component.getTupleIndex());
        break;
      case KeyPathExpr::Component::Kind::DictionaryKey:
        attribute("unresolvedName", component.getUnresolvedDeclName());
        break;
      case KeyPathExpr::Component::Kind::Invalid:
      case KeyPathExpr::Component::Kind::OptionalForce:
      case KeyPathExpr::Component::Kind::OptionalChain:
      case KeyPathExpr::Component::Kind::OptionalWrap:
      case KeyPathExpr::Component::Kind::Identity:
      case KeyPathExpr::Component::Kind::CodeCompletion:
        // Nothing to write;
        break;
      }
    });
  }

  void value(const LayoutConstraint &layout) {
    json.object([&] {
      attribute("kind", jsonStringForLayoutConstraintKind(layout->getKind()));
      // If we ever dump `@_specialize` attributes, we should include the
      // size and alignment here.
    });
  }

  template <typename T>
  void value(const Located<T> &v) {
    value(v.Item);
  }

  void value(ObjCSelector selector) {
    json.object([&] {
      attribute("numArgs", selector.getNumArgs());
      attributeArray("pieces", selector.getSelectorPieces());
    });
  }

  template <typename T>
  void value(const std::optional<T> &v) {
    if (!v.has_value()) {
      return;
    }
    value(*v);
  }

  void value(const PrecedenceGroupDecl::Relation &relation) {
    json.object([&] { attribute("name", relation.Name); });
  }

  void value(const PoundAvailableInfo *info) {
    json.object([&] {
      attributeBool("isUnavailability", info->isUnavailability());
      attributeArray("queries", info->getQueries());
    });
  }

  void value(const PoundHasSymbolInfo *info) {
    json.object([&] { attribute("symbolExpr", info->getSymbolExpr()); });
  }

  void value(const ProtocolConformanceRef &ref) {
    json.object([&] {
      if (ref.isAbstract()) {
        attributeString("kind", "abstract");
        attributeString("protocolDeclUSR", declUSR(ref.getAbstract()));
      } else if (ref.isConcrete()) {
        ProtocolConformance *conf = ref.getConcrete();
        attributeString("kind", "concrete");
        attributeString("conformingTypeUSR", typeUSR(conf->getType()));
        attributeString("protocolDeclUSR", declUSR(conf->getProtocol()));
      } else if (ref.isPack()) {
        PackConformance *conf = ref.getPack();
        attributeString("kind", "pack");
        attributeString("conformingTypeUSR", typeUSR(conf->getType()));
        attributeString("protocolDeclUSR", declUSR(conf->getProtocol()));
      }
    });
  }

  void value(ProtocolTypeAlias alias) {
    json.object([&] {
      attribute("name", alias.getName());
      attribute("underlyingTypeUSR", typeUSR(alias.getUnderlyingType()));
    });
  }

  void value(const Requirement &req) {
    json.object([&] {
      attribute("kind", jsonStringForRequirementKind(req.getKind()));
      attribute("first", typeUSR(req.getFirstType()));
      switch (req.getKind()) {
      case RequirementKind::Layout:
        attribute("layout", req.getLayoutConstraint());
        break;
      default:
        attribute("second", typeUSR(req.getSecondType()));
      }
    });
  }

  void value(RequirementSignature sig) {
    json.object([&] {
      attributeArray("requirements", sig.getRequirements());
      attributeArray("typeAliases", sig.getTypeAliases());
    });
  }

  // Written this way to match *only* `StmtConditionElement` and prevent
  // overload resolution from attempting to match other types via its
  // constructors.
  template <typename T, typename std::enable_if<std::is_same<
                            T, StmtConditionElement>::value>::type * = nullptr>
  void value(const T &element) {
    json.object([&] {
      attributeString("kind",
                      jsonStringForStmtConditionKind(element.getKind()));
      switch (element.getKind()) {
      case StmtConditionElement::CK_Boolean:
        attributeBool("expr", element.getBoolean());
        break;
      case StmtConditionElement::CK_PatternBinding:
        attribute("pattern", element.getPattern());
        attribute("initializer", element.getInitializer());
        break;
      case StmtConditionElement::CK_Availability:
        attribute("availability", element.getAvailability());
        break;
      case StmtConditionElement::CK_HasSymbol:
        attribute("symbol", element.getHasSymbolInfo());
        break;
      }
    });
  }

  void value(const SubstitutionMap &subst) {
    GenericSignature sig = subst.getGenericSignature();
    json.object([&] {
      if (!sig.getGenericParams().empty()) {
        json.attributeArray("parameters", [&] {
          for (auto param : sig.getGenericParams()) {
            json.value(typeUSR(param));
          }
        });
      }
      if (!subst.getReplacementTypes().empty()) {
        json.attributeArray("replacements", [&] {
          for (auto ty : subst.getReplacementTypes()) {
            json.value(typeUSR(ty));
          }
        });
      }
      if (!sig.getRequirements().empty()) {
        json.attributeArray("requirements", [&] {
          for (auto req : sig.getRequirements()) {
            value(req);
          }
        });
      }
      if (!subst.getConformances().empty()) {
        json.attributeArray("conformances", [&] {
          for (auto conf : subst.getConformances()) {
            value(conf);
          }
        });
      }
    });
  }

  void value(const ThrownErrorDestination &dest) {
    json.object([&] {
      attribute("thrownErrorTypeUSR", typeUSR(dest.getThrownErrorType()));
    });
  }

  void value(TuplePatternElt element) {
    json.object([&] {
      attribute("label", element.getLabel());
      attribute("pattern", element.getPattern());
    });
  }

  void value(TypeOrCustomAttr attr) {
    if (auto A = attr.dyn_cast<TypeAttribute *>()) {
      value(A);
    } else if (auto A = attr.dyn_cast<CustomAttr *>()) {
      value(A);
    }
  }

  void value(llvm::VersionTuple version) { value(version.getAsString()); }

  /// Writes a Boolean-valued attribute into the current object, only if the
  /// value is true.
  void attributeBool(StringRef k, bool v) {
    if (!v)
      return;
    json.attribute(k, v);
  }

  /// Writes a string-valued attribute into the current object, only if the
  /// value is non-empty.
  void attributeString(StringRef k, StringRef v) {
    if (v.empty())
      return;
    json.attribute(k, v);
  }

  /// Unconditionally writes an attribute with the given key and value into
  /// the current object.
  template <typename T, typename std::enable_if<!std::is_constructible<
                            bool, T>::value>::type * = nullptr>
  void attribute(StringRef k, const T &v) {
    json.attributeBegin(k);
    value(v);
    json.attributeEnd();
  }

  /// Conditionally writes an attribute with the given key and value into the
  /// current object. If the value -- which can be converted to `bool`
  /// implicitly or explicitly -- is false, the attribute is not written.
  template <typename T,
            typename std::enable_if<std::is_constructible<bool, T>::value>::type
                * = nullptr>
  void attribute(StringRef k, const T &v) {
    if (!v)
      return;
    json.attributeBegin(k);
    value(v);
    json.attributeEnd();
  }

  /// Conditionally writes an attribute with the given key and value into the
  /// current object. The value is a sequence that is written as a JSON array.
  void attributeArray(StringRef k, const ArgumentList *args) {
    if (args == nullptr || args->empty())
      return;

    json.attributeArray(k, [&] {
      for (auto arg : *args) {
        value(arg);
      }
    });
  }

  /// Conditionally writes an attribute with the given key and value into the
  /// current object. The value is a sequence that is written as a JSON array.
  template <typename T,
            typename std::enable_if<is_iterable<T>::value>::type * = nullptr>
  void attributeArray(StringRef k, T &&elements) {
    if (elements.begin() == elements.end())
      return;

    json.attributeArray(k, [&] {
      for (auto element : elements) {
        value(element);
      }
    });
  }

  void writeAttr(DeclAttribute *attr, DeclContext *DC) {
    json.object([&] {
      if (const CustomAttr *customAttr = dyn_cast<CustomAttr>(attr)) {
        attributeString("typeUSR", typeUSR(customAttr->getType()));
        attributeArray("args", customAttr->getArgs());
      } else {
        attributeString("attrName", attr->getAttrName());
        if (const ImplementsAttr *A = dyn_cast<ImplementsAttr>(attr)) {
          attributeString("protocolDeclUSR", declUSR(A->getProtocol(DC)));
          attribute("memberName", A->getMemberName());
        }
      }
    });
  }

  void writeKindAttribute(ASTNode node) {
    if (Decl *D = node.dyn_cast<Decl *>()) {
      attributeString("kind",
                      std::string(Decl::getKindName(D->getKind())) + "Decl");
    } else if (Expr *E = node.dyn_cast<Expr *>()) {
      attributeString("kind",
                      std::string(Expr::getKindName(E->getKind())) + "Expr");
    } else if (Stmt *S = node.dyn_cast<Stmt *>()) {
      attributeString("kind",
                      std::string(Stmt::getKindName(S->getKind())) + "Stmt");
    } else if (Pattern *P = node.dyn_cast<Pattern *>()) {
      attributeString("kind", std::string(Pattern::getKindName(P->getKind())) +
                                  "Pattern");
    }
  }

  void writeSourceRange(ASTNode node) {
    SourceLoc sloc = node.getStartLoc();
    SourceLoc eloc = node.getEndLoc();
    if (sloc.isInvalid() || eloc.isInvalid())
      return;

    SourceManager &srcMgr = ctx.SourceMgr;
    unsigned startBufferID = srcMgr.findBufferContainingLoc(sloc);
    unsigned startOffset = srcMgr.getLocOffsetInBuffer(sloc, startBufferID);
    unsigned endBufferID = srcMgr.findBufferContainingLoc(eloc);
    unsigned endOffset = srcMgr.getLocOffsetInBuffer(eloc, endBufferID);
    json.attributeArray("sourceRange", [&] {
      json.value(startOffset);
      json.value(endOffset);
    });
  }
};

} // end anonymous namespace

void SourceFile::dumpJSON(ASTContext &ctx, llvm::raw_ostream &os) {
  JSONASTVisitor(os, ctx).visitSourceFile(*this);
}
