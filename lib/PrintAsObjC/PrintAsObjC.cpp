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
#include "swift/Strings.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/AST/SwiftNameTranslation.h"
#include "swift/AST/Comment.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Basic/Version.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/CommentConversion.h"
#include "swift/Parse/Lexer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/Module.h"
#include "clang/Lex/Lexer.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace swift::objc_translation;

static bool isNSObjectOrAnyHashable(ASTContext &ctx, Type type) {
  if (auto classDecl = type->getClassOrBoundGenericClass()) {
    return classDecl->getName()
             == ctx.getSwiftId(KnownFoundationEntity::NSObject) &&
           classDecl->getModuleContext()->getName() == ctx.Id_ObjectiveC;
  }
  if (auto nomDecl = type->getAnyNominal()) {
    return nomDecl == ctx.getAnyHashableDecl();
  }

  return false;
}

static bool isAnyObjectOrAny(Type type) {
  return type->isAnyObject() || type->isAny();
}

/// Returns true if \p name matches a keyword in any Clang language mode.
static bool isClangKeyword(Identifier name) {
  static const llvm::StringSet<> keywords = []{
    llvm::StringSet<> set;
    // FIXME: clang::IdentifierInfo /nearly/ has the API we need to do this
    // in a more principled way, but not quite.
#define KEYWORD(SPELLING, FLAGS) \
    set.insert(#SPELLING);
#define CXX_KEYWORD_OPERATOR(SPELLING, TOK) \
    set.insert(#SPELLING);
#include "clang/Basic/TokenKinds.def"
    return set;
  }();

  if (name.empty())
    return false;
  return keywords.find(name.str()) != keywords.end();
}


namespace {
  /// Whether the type being printed is in function param position.
  enum IsFunctionParam_t : bool {
    IsFunctionParam = true,
    IsNotFunctionParam = false,
  };
} // end anonymous namespace

/// Returns true if the given selector might be classified as an init method
/// by Objective-C ARC.
static bool looksLikeInitMethod(ObjCSelector selector) {
  ArrayRef<Identifier> selectorPieces = selector.getSelectorPieces();
  assert(!selectorPieces.empty());
  auto firstPiece = selectorPieces.front().str();
  if (!firstPiece.startswith("init")) return false;
  return !(firstPiece.size() > 4 && clang::isLowercase(firstPiece[4]));
}

/// Returns the name of an <os/object.h> type minus the leading "OS_",
/// or an empty string if \p decl is not an <os/object.h> type.
static StringRef maybeGetOSObjectBaseName(const clang::NamedDecl *decl) {
  StringRef name = decl->getName();
  if (!name.consume_front("OS_"))
    return StringRef();

  clang::SourceLocation loc = decl->getLocation();
  if (!loc.isMacroID())
    return StringRef();

  // Hack: check to see if the name came from a macro in <os/object.h>.
  clang::SourceManager &sourceMgr = decl->getASTContext().getSourceManager();
  clang::SourceLocation expansionLoc =
      sourceMgr.getImmediateExpansionRange(loc).first;
  clang::SourceLocation spellingLoc = sourceMgr.getSpellingLoc(expansionLoc);

  if (!sourceMgr.getFilename(spellingLoc).endswith("/os/object.h"))
    return StringRef();

  return name;
}

/// Returns true if \p decl represents an <os/object.h> type.
static bool isOSObjectType(const clang::Decl *decl) {
  auto *named = dyn_cast_or_null<clang::NamedDecl>(decl);
  if (!named)
    return false;
  return !maybeGetOSObjectBaseName(named).empty();
}


namespace {
using DelayedMemberSet = llvm::SmallSetVector<const ValueDecl *, 32>;

class ObjCPrinter : private DeclVisitor<ObjCPrinter>,
                    private TypeVisitor<ObjCPrinter, void, 
                                        Optional<OptionalTypeKind>>
{
  friend ASTVisitor;
  friend TypeVisitor;

  using NameAndOptional = std::pair<StringRef, bool>;
  llvm::DenseMap<std::pair<Identifier, Identifier>, NameAndOptional>
    specialNames;
  Identifier ID_CFTypeRef;

  ModuleDecl &M;
  raw_ostream &os;

  SmallVector<const FunctionType *, 4> openFunctionTypes;
  const DelayedMemberSet &delayedMembers;

  AccessLevel minRequiredAccess;
  bool protocolMembersOptional = false;

  Optional<Type> NSCopyingType;
  
  friend ASTVisitor<ObjCPrinter>;
  friend TypeVisitor<ObjCPrinter>;

public:
  explicit ObjCPrinter(ModuleDecl &mod, raw_ostream &out,
                       DelayedMemberSet &delayed, AccessLevel access)
    : M(mod), os(out), delayedMembers(delayed), minRequiredAccess(access) {}

  void print(const Decl *D) {
    PrettyStackTraceDecl trace("printing", D);
    ASTVisitor::visit(const_cast<Decl *>(D));
  }

  void maybePrintObjCGenericParameters(const ClassDecl *importedClass) {
    auto *clangDecl = importedClass->getClangDecl();
    auto *objcClass = dyn_cast_or_null<clang::ObjCInterfaceDecl>(clangDecl);
    if (!objcClass)
      return;
    if (!objcClass->getTypeParamList())
      return;
    assert(objcClass->getTypeParamList()->size() != 0);
    os << "<";
    interleave(*objcClass->getTypeParamList(),
               [this](const clang::ObjCTypeParamDecl *param) {
                 os << param->getName();
               },
               [this] { os << ", "; });
    os << ">";
  }

  void printAdHocCategory(iterator_range<const ValueDecl * const *> members) {
    assert(members.begin() != members.end());

    const DeclContext *origDC = (*members.begin())->getDeclContext();
    auto *baseClass = dyn_cast<ClassDecl>(origDC);
    if (!baseClass) {
      Type extendedTy = cast<ExtensionDecl>(origDC)->getExtendedType();
      baseClass = extendedTy->getClassOrBoundGenericClass();
    }

    os << "@interface " << getNameForObjC(baseClass);
    maybePrintObjCGenericParameters(baseClass);
    os << " (SWIFT_EXTENSION(" << origDC->getParentModule()->getName()
       << "))\n";
    printMembers</*allowDelayed*/true>(members);
    os << "@end\n\n";
  }
  
  bool shouldInclude(const ValueDecl *VD) {
    return isVisibleToObjC(VD, minRequiredAccess);
  }

private:
  /// Prints a protocol adoption list: <code>&lt;NSCoding, NSCopying&gt;</code>
  ///
  /// This method filters out non-ObjC protocols.
  void printProtocols(ArrayRef<ProtocolDecl *> protos) {
    SmallVector<ProtocolDecl *, 4> protosToPrint;
    std::copy_if(protos.begin(), protos.end(),
                 std::back_inserter(protosToPrint),
                 [this](const ProtocolDecl *PD) -> bool {
      return shouldInclude(PD);
    });

    // Drop protocols from the list that are implied by other protocols.
    ProtocolType::canonicalizeProtocols(protosToPrint);

    if (protosToPrint.empty())
      return;

    os << " <";
    interleave(protosToPrint,
               [this](const ProtocolDecl *PD) { os << getNameForObjC(PD); },
               [this] { os << ", "; });
    os << ">";
  }

  /// Prints the members of a class, extension, or protocol.
  template <bool AllowDelayed = false, typename R>
  void printMembers(R &&members) {
    for (const Decl *member : members) {
      auto VD = dyn_cast<ValueDecl>(member);
      if (!VD || !shouldInclude(VD) || isa<TypeDecl>(VD))
        continue;
      if (isa<AccessorDecl>(VD))
        continue;
      if (!AllowDelayed && delayedMembers.count(VD)) {
        os << "// '" << VD->getFullName() << "' below\n";
        continue;
      }
      if (VD->getAttrs().hasAttribute<OptionalAttr>() != protocolMembersOptional) {
        protocolMembersOptional = VD->getAttrs().hasAttribute<OptionalAttr>();
        os << (protocolMembersOptional ? "@optional\n" : "@required\n");
      }
      ASTVisitor::visit(const_cast<ValueDecl*>(VD));
    }
  }

  void printDocumentationComment(Decl *D) {
    swift::markup::MarkupContext MC;
    auto DC = getSingleDocComment(MC, D);
    if (DC.hasValue())
      ide::getDocumentationCommentAsDoxygen(DC.getValue(), os);
  }

  /// Prints an encoded string, escaped properly for C.
  void printEncodedString(StringRef str, bool includeQuotes = true) {
    // NB: We don't use raw_ostream::write_escaped() because it does hex escapes
    // for non-ASCII chars.

    llvm::SmallString<128> Buf;
    StringRef decodedStr = Lexer::getEncodedStringSegment(str, Buf);

    if (includeQuotes) os << '"';
    for (unsigned char c : decodedStr) {
      switch (c) {
      case '\\':
        os << '\\' << '\\';
        break;
      case '\t':
        os << '\\' << 't';
        break;
      case '\n':
        os << '\\' << 'n';
        break;
      case '"':
        os << '\\' << '"';
        break;
      default:
        if (c < 0x20 || c == 0x7F) {
          os << '\\' << 'x';
          os << llvm::hexdigit((c >> 4) & 0xF);
          os << llvm::hexdigit((c >> 0) & 0xF);
        } else {
          os << c;
        }
      }
    }
    if (includeQuotes) os << '"';
  }

  // For a given Decl and Type, if the type is not an optional return
  // the type and OTK_None as the optionality. If the type is
  // optional, return the underlying object type, and an optionality
  // that is based on the type but overridden by the
  // ImplicitlyUnwrappedOptionalAttr on the decl.
  static std::pair<Type, OptionalTypeKind>
  getObjectTypeAndOptionality(const Decl *D, Type ty) {
    OptionalTypeKind kind;
    if (auto objTy =
            ty->getReferenceStorageReferent()->getOptionalObjectType()) {
      kind = OTK_Optional;
      if (D->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>())
        kind = OTK_ImplicitlyUnwrappedOptional;

      return {objTy, kind};
    }

    return {ty, OTK_None};
  }

  // Ignore other declarations.
  void visitDecl(Decl *D) {}

  void visitClassDecl(ClassDecl *CD) {
    printDocumentationComment(CD);

    StringRef customName = getNameForObjC(CD, CustomNamesOnly);
    if (customName.empty()) {
      llvm::SmallString<32> scratch;
      os << "SWIFT_CLASS(\"" << CD->getObjCRuntimeName(scratch) << "\")";
      printAvailability(CD);
      os << "\n@interface " << CD->getName();
    } else {
      os << "SWIFT_CLASS_NAMED(\"" << CD->getName() << "\")";
      printAvailability(CD);
      os << "\n@interface " << customName;
    }

    if (auto superDecl = CD->getSuperclassDecl())
      os << " : " << getNameForObjC(superDecl);
    printProtocols(CD->getLocalProtocols(ConformanceLookupKind::OnlyExplicit));
    os << "\n";
    printMembers(CD->getMembers());
    os << "@end\n";
  }

  bool isEmptyExtensionDecl(ExtensionDecl *ED) {
    auto members = ED->getMembers();
    auto hasMembers = std::any_of(members.begin(), members.end(),
                                  [this](const Decl *D) -> bool {
      if (auto VD = dyn_cast<ValueDecl>(D))
        if (shouldInclude(VD))
          return true;
      return false;
    });

    auto protocols = ED->getLocalProtocols(ConformanceLookupKind::OnlyExplicit);
    auto hasProtocols = std::any_of(protocols.begin(), protocols.end(),
                                    [this](const ProtocolDecl *PD) -> bool {
      return shouldInclude(PD);
    });

    return (!hasMembers && !hasProtocols);
  }

  void visitExtensionDecl(ExtensionDecl *ED) {
    if (isEmptyExtensionDecl(ED))
      return;

    auto baseClass = ED->getExtendedType()->getClassOrBoundGenericClass();

    if (printAvailability(ED, PrintLeadingSpace::No))
      os << "\n";
    os << "@interface " << getNameForObjC(baseClass);
    maybePrintObjCGenericParameters(baseClass);
    os << " (SWIFT_EXTENSION(" << ED->getModuleContext()->getName() << "))";
    printProtocols(ED->getLocalProtocols(ConformanceLookupKind::OnlyExplicit));
    os << "\n";
    printMembers(ED->getMembers());
    os << "@end\n";
  }

  void visitProtocolDecl(ProtocolDecl *PD) {
    printDocumentationComment(PD);

    StringRef customName = getNameForObjC(PD, CustomNamesOnly);
    if (customName.empty()) {
      llvm::SmallString<32> scratch;
      os << "SWIFT_PROTOCOL(\"" << PD->getObjCRuntimeName(scratch) << "\")";
      printAvailability(PD);
      os << "\n@protocol " << PD->getName();
    } else {
      os << "SWIFT_PROTOCOL_NAMED(\"" << PD->getName() << "\")";
      printAvailability(PD);
      os << "\n@protocol " << customName;
    }

    printProtocols(PD->getInheritedProtocols());
    os << "\n";
    assert(!protocolMembersOptional && "protocols start required");
    printMembers(PD->getMembers());
    protocolMembersOptional = false;
    os << "@end\n";
  }
  
  void visitEnumDecl(EnumDecl *ED) {
    printDocumentationComment(ED);
    os << "typedef ";
    StringRef customName = getNameForObjC(ED, CustomNamesOnly);
    if (customName.empty()) {
      os << "SWIFT_ENUM(";
    } else {
      os << "SWIFT_ENUM_NAMED(";
    }
    print(ED->getRawType(), OTK_None);
    if (customName.empty()) {
      os << ", " << ED->getName();
    } else {
      os << ", " << customName
         << ", \"" << ED->getName() << "\"";
    }
    os << ", "
       << (ED->isFormallyExhaustive(/*useDC*/nullptr) ? "closed" : "open")
       << ") {\n";

    for (auto Elt : ED->getAllElements()) {
      printDocumentationComment(Elt);

      // Print the cases as the concatenation of the enum name with the case
      // name.
      os << "  ";
      if (printSwiftEnumElemNameInObjC(Elt, os)) {
        os << " SWIFT_COMPILE_NAME(\"" << Elt->getName() << "\")";
      }
      
      if (auto ILE = cast_or_null<IntegerLiteralExpr>(Elt->getRawValueExpr())) {
        os << " = ";
        if (ILE->isNegative())
          os << "-";
        os << ILE->getDigitsText();
      }
      os << ",\n";
    }
    os << "};\n";
  }

  void printSingleMethodParam(StringRef selectorPiece,
                              const ParamDecl *param,
                              const clang::ParmVarDecl *clangParam,
                              bool isNSUIntegerSubscript,
                              bool isLastPiece) {
    os << selectorPiece << ":(";
    if ((isNSUIntegerSubscript && isLastPiece) ||
        (clangParam && isNSUInteger(clangParam->getType()))) {
      os << "NSUInteger";
    } else {
      OptionalTypeKind kind;
      Type objTy;
      std::tie(objTy, kind) =
          getObjectTypeAndOptionality(param, param->getInterfaceType());
      print(objTy, kind, Identifier(), IsFunctionParam);
    }
    os << ")";

    if (!param->hasName()) {
      os << "_";
    } else {
      Identifier name = param->getName();
      os << name;
      if (isClangKeyword(name))
        os << "_";
    }
  }

  template <typename T>
  static const T *findClangBase(const T *member) {
    while (member) {
      if (member->getClangDecl())
        return member;
      member = member->getOverriddenDecl();
    }
    return nullptr;
  }

  /// Returns true if \p clangTy is the typedef for NSUInteger.
  bool isNSUInteger(clang::QualType clangTy) {
    const auto *typedefTy = dyn_cast<clang::TypedefType>(clangTy);
    if (!typedefTy)
      return false;

    const clang::IdentifierInfo *nameII = typedefTy->getDecl()->getIdentifier();
    if (!nameII)
      return false;
    if (nameII->getName() != "NSUInteger")
      return false;

    return true;
  }

  Type getForeignResultType(AbstractFunctionDecl *AFD,
                            FunctionType *methodTy,
                            Optional<ForeignErrorConvention> errorConvention) {
    // A foreign error convention can affect the result type as seen in
    // Objective-C.
    if (errorConvention) {
      switch (errorConvention->getKind()) {
      case ForeignErrorConvention::ZeroResult:
      case ForeignErrorConvention::NonZeroResult:
        // The error convention provides the result type.
        return errorConvention->getResultType();

      case ForeignErrorConvention::NilResult:
        // Errors are propagated via 'nil' returns.
        return OptionalType::get(methodTy->getResult());

      case ForeignErrorConvention::NonNilError:
      case ForeignErrorConvention::ZeroPreservedResult:
        break;
      }
    }

    auto result = methodTy->getResult();
    if (result->isUninhabited())
      return M.getASTContext().TheEmptyTupleType;
    return result;
  }

  /// Returns true if \p sel is the no-argument selector 'init'.
  static bool selectorIsInit(ObjCSelector sel) {
    return sel.getNumArgs() == 0 &&
           sel.getSelectorPieces().front().str() == "init";
  }
                                          
  void printAbstractFunctionAsMethod(AbstractFunctionDecl *AFD,
                                     bool isClassMethod,
                                     bool isNSUIntegerSubscript = false) {
    printDocumentationComment(AFD);
    if (isClassMethod)
      os << "+ (";
    else
      os << "- (";

    const clang::ObjCMethodDecl *clangMethod = nullptr;
    if (!isNSUIntegerSubscript) {
      if (const AbstractFunctionDecl *clangBase = findClangBase(AFD)) {
        clangMethod =
            dyn_cast_or_null<clang::ObjCMethodDecl>(clangBase->getClangDecl());
      }
    }

    Optional<ForeignErrorConvention> errorConvention
      = AFD->getForeignErrorConvention();
    Type rawMethodTy = AFD->getMethodInterfaceType();
    auto methodTy = rawMethodTy->castTo<FunctionType>();
    auto resultTy = getForeignResultType(AFD, methodTy, errorConvention);

    // Constructors and methods returning DynamicSelf return
    // instancetype.    
    if (isa<ConstructorDecl>(AFD) ||
        (isa<FuncDecl>(AFD) && cast<FuncDecl>(AFD)->hasDynamicSelf())) {
      if (errorConvention && errorConvention->stripsResultOptionality()) {
        printNullability(OTK_Optional, NullabilityPrintKind::ContextSensitive);
      } else if (auto ctor = dyn_cast<ConstructorDecl>(AFD)) {
        printNullability(ctor->getFailability(),
                         NullabilityPrintKind::ContextSensitive);
      } else {
        auto func = cast<FuncDecl>(AFD);
        OptionalTypeKind kind;
        Type objTy;
        std::tie(objTy, kind) =
          getObjectTypeAndOptionality(func, func->getResultInterfaceType());

        printNullability(kind,
                         NullabilityPrintKind::ContextSensitive);
      }

      os << "instancetype";
    } else if (resultTy->isVoid() &&
               AFD->getAttrs().hasAttribute<IBActionAttr>()) {
      os << "IBAction";
    } else if (clangMethod && isNSUInteger(clangMethod->getReturnType())) {
      os << "NSUInteger";
    } else {
      OptionalTypeKind kind;
      Type objTy;
      std::tie(objTy, kind) = getObjectTypeAndOptionality(AFD, resultTy);
      print(objTy, kind);
    }

    os << ")";

    auto selector = AFD->getObjCSelector();
    ArrayRef<Identifier> selectorPieces = selector.getSelectorPieces();
    
    const auto &params = AFD->getParameters()->getArray();
    unsigned paramIndex = 0;
    for (unsigned i = 0, n = selectorPieces.size(); i != n; ++i) {
      if (i > 0) os << ' ';

      // Retrieve the selector piece.
      StringRef piece = selectorPieces[i].empty() ? StringRef("")
                                                  : selectorPieces[i].str();

      // If we have an error convention and this is the error
      // parameter, print it.
      if (errorConvention && i == errorConvention->getErrorParameterIndex()) {
        os << piece << ":(";
        print(errorConvention->getErrorParameterType(), None);
        os << ")error";
        continue;
      }

      // Zero-parameter initializers with a long selector.
      if (isa<ConstructorDecl>(AFD) &&
          cast<ConstructorDecl>(AFD)->isObjCZeroParameterWithLongSelector()) {
        os << piece;
        continue;
      }

      // Zero-parameter methods.
      if (params.empty()) {
        assert(paramIndex == 0);
        os << piece;
        paramIndex = 1;
        continue;
      }

      const clang::ParmVarDecl *clangParam = nullptr;
      if (clangMethod)
        clangParam = clangMethod->parameters()[paramIndex];

      // Single-parameter methods.
      printSingleMethodParam(piece, params[paramIndex], clangParam,
                             isNSUIntegerSubscript, i == n-1);
      ++paramIndex;
    }

    bool skipAvailability = false;
    bool makeNewUnavailable = false;
    bool makeNewExplicitlyAvailable = false;
    // Swift designated initializers are Objective-C designated initializers.
    if (auto ctor = dyn_cast<ConstructorDecl>(AFD)) {
      if (ctor->hasStubImplementation()
          || ctor->getFormalAccess() < minRequiredAccess) {
        // This will only be reached if the overridden initializer has the
        // required access
        os << " SWIFT_UNAVAILABLE";
        skipAvailability = true;
        // If -init is unavailable, then +new should be, too:
        makeNewUnavailable = selectorIsInit(selector);
      } else {
        if (ctor->isDesignatedInit() &&
            !isa<ProtocolDecl>(ctor->getDeclContext())) {
          os << " OBJC_DESIGNATED_INITIALIZER";
        }

        // If -init is newly available, +new should be as well if the class
        // inherits from NSObject.
        if (selectorIsInit(selector) && !ctor->getOverriddenDecl()) {
          auto container = ctor->getDeclContext();
          auto *classDecl = container->getAsClassOrClassExtensionContext();
          if (!classDecl) {
            assert(container->getAsProtocolOrProtocolExtensionContext());
          } else {
            while (classDecl->hasSuperclass()) {
              classDecl = classDecl->getSuperclassDecl();
              assert(classDecl &&
                     "shouldn't PrintAsObjC with invalid superclasses");
            }
            if (classDecl->hasClangNode() &&
                classDecl->getNameStr() == "NSObject") {
              makeNewExplicitlyAvailable = true;
            }
          }
        }
      }

      if (!looksLikeInitMethod(AFD->getObjCSelector())) {
        os << " SWIFT_METHOD_FAMILY(init)";
      }
    } else {
      if (looksLikeInitMethod(AFD->getObjCSelector())) {
        os << " SWIFT_METHOD_FAMILY(none)";
      }
      if (methodTy->getResult()->isUninhabited()) {
        os << " SWIFT_NORETURN";
      } else if (!methodTy->getResult()->isVoid() &&
                 !AFD->getAttrs().hasAttribute<DiscardableResultAttr>()) {
        os << " SWIFT_WARN_UNUSED_RESULT";
      }
    }

    if (!skipAvailability) {
      printAvailability(AFD);
    }

    if (auto accessor = dyn_cast<AccessorDecl>(AFD)) {
      printSwift3ObjCDeprecatedInference(accessor->getStorage());
    } else {
      printSwift3ObjCDeprecatedInference(AFD);
    }

    os << ";\n";

    if (makeNewUnavailable) {
      assert(!makeNewExplicitlyAvailable);
      // Downgrade this to a warning in pre-Swift-5 mode. This isn't perfect
      // because it's a diagnostic inflicted on /clients/, but it's close
      // enough. It really is invalid to call +new when -init is unavailable.
      StringRef annotationName = "SWIFT_UNAVAILABLE_MSG";
      if (!M.getASTContext().isSwiftVersionAtLeast(5))
        annotationName = "SWIFT_DEPRECATED_MSG";
      os << "+ (nonnull instancetype)new " << annotationName
         << "(\"-init is unavailable\");\n";
    } else if (makeNewExplicitlyAvailable) {
      os << "+ (nonnull instancetype)new;\n";
    }
  }

  void printAbstractFunctionAsFunction(FuncDecl *FD) {
    printDocumentationComment(FD);
    Optional<ForeignErrorConvention> errorConvention
      = FD->getForeignErrorConvention();
    assert(!FD->getGenericSignature() &&
           "top-level generic functions not supported here");
    auto funcTy = FD->getInterfaceType()->castTo<FunctionType>();
    auto resultTy = getForeignResultType(FD, funcTy, errorConvention);
    
    // The result type may be a partial function type we need to close
    // up later.
    PrintMultiPartType multiPart(*this);

    OptionalTypeKind kind;
    Type objTy;
    std::tie(objTy, kind) = getObjectTypeAndOptionality(FD, resultTy);
    visitPart(objTy, kind);

    assert(FD->getAttrs().hasAttribute<CDeclAttr>()
           && "not a cdecl function");
    
    os << ' ' << FD->getAttrs().getAttribute<CDeclAttr>()->Name << '(';
    
    auto params = FD->getParameters();
    if (params->size()) {
      interleave(*params,
                 [&](const ParamDecl *param) {
                   OptionalTypeKind kind;
                   Type objTy;
                   std::tie(objTy, kind) = getObjectTypeAndOptionality(
                       param, param->getInterfaceType());
                   print(objTy, kind, param->getName(), IsFunctionParam);
                 },
                 [&] { os << ", "; });
    } else {
      os << "void";
    }
    
    os << ')';
    
    // Finish the result type.
    multiPart.finish();

    if (funcTy->getResult()->isUninhabited()) {
      os << " SWIFT_NORETURN";
    } else if (!funcTy->getResult()->isVoid() &&
               !FD->getAttrs().hasAttribute<DiscardableResultAttr>()) {
      os << " SWIFT_WARN_UNUSED_RESULT";
    }

    printAvailability(FD);
    
    os << ';';
  }

  enum class PrintLeadingSpace : bool {
    No = false,
    Yes = true
  };

  /// Returns \c true if anything was printed.
  bool printAvailability(
      const Decl *D,
      PrintLeadingSpace printLeadingSpace = PrintLeadingSpace::Yes) {
    bool hasPrintedAnything = false;
    auto maybePrintLeadingSpace = [&] {
      if (printLeadingSpace == PrintLeadingSpace::Yes || hasPrintedAnything)
        os << " ";
      hasPrintedAnything = true;
    };

    for (auto AvAttr : D->getAttrs().getAttributes<AvailableAttr>()) {
      if (AvAttr->Platform == PlatformKind::none) {
        if (AvAttr->PlatformAgnostic == PlatformAgnosticAvailabilityKind::Unavailable) {
          // Availability for *
          if (!AvAttr->Rename.empty() && isa<ValueDecl>(D)) {
            // rename
            maybePrintLeadingSpace();
            os << "SWIFT_UNAVAILABLE_MSG(\"'"
               << cast<ValueDecl>(D)->getBaseName()
               << "' has been renamed to '";
            printRenameForDecl(AvAttr, cast<ValueDecl>(D), false);
            os << '\'';
            if (!AvAttr->Message.empty()) {
              os << ": ";
              printEncodedString(AvAttr->Message, false);
            }
            os << "\")";
          } else if (!AvAttr->Message.empty()) {
            maybePrintLeadingSpace();
            os << "SWIFT_UNAVAILABLE_MSG(";
            printEncodedString(AvAttr->Message);
            os << ")";
          } else {
            maybePrintLeadingSpace();
            os << "SWIFT_UNAVAILABLE";
          }
          break;
        }
        if (AvAttr->isUnconditionallyDeprecated()) {
          if (!AvAttr->Rename.empty() || !AvAttr->Message.empty()) {
            maybePrintLeadingSpace();
            os << "SWIFT_DEPRECATED_MSG(";
            printEncodedString(AvAttr->Message);
            if (!AvAttr->Rename.empty()) {
              os << ", ";
              printRenameForDecl(AvAttr, cast<ValueDecl>(D), true);
            }
            os << ")";
          } else {
            maybePrintLeadingSpace();
            os << "SWIFT_DEPRECATED";
          }
        }
        continue;
      }

      // Availability for a specific platform
      if (!AvAttr->Introduced.hasValue()
          && !AvAttr->Deprecated.hasValue()
          && !AvAttr->Obsoleted.hasValue()
          && !AvAttr->isUnconditionallyDeprecated()
          && !AvAttr->isUnconditionallyUnavailable()) {
        continue;
      }

      const char *plat;
      switch (AvAttr->Platform) {
      case PlatformKind::OSX:
        plat = "macos";
        break;
      case PlatformKind::iOS:
        plat = "ios";
        break;
      case PlatformKind::tvOS:
        plat = "tvos";
        break;
      case PlatformKind::watchOS:
        plat = "watchos";
        break;
      case PlatformKind::OSXApplicationExtension:
        plat = "macos_app_extension";
        break;
      case PlatformKind::iOSApplicationExtension:
        plat = "ios_app_extension";
        break;
      case PlatformKind::tvOSApplicationExtension:
        plat = "tvos_app_extension";
        break;
      case PlatformKind::watchOSApplicationExtension:
        plat = "watchos_app_extension";
        break;
      case PlatformKind::none:
        llvm_unreachable("handled above");
      }

      maybePrintLeadingSpace();
      os << "SWIFT_AVAILABILITY(" << plat;
      if (AvAttr->isUnconditionallyUnavailable()) {
        os << ",unavailable";
      } else {
        if (AvAttr->Introduced.hasValue()) {
          os << ",introduced=" << AvAttr->Introduced.getValue().getAsString();
        }
        if (AvAttr->Deprecated.hasValue()) {
          os << ",deprecated=" << AvAttr->Deprecated.getValue().getAsString();
        } else if (AvAttr->isUnconditionallyDeprecated()) {
          // We need to specify some version, we can't just say deprecated.
          // We also can't deprecate it before it's introduced.
          if (AvAttr->Introduced.hasValue()) {
            os << ",deprecated=" << AvAttr->Introduced.getValue().getAsString();
          } else {
            os << ",deprecated=0.0.1";
          }
        }
        if (AvAttr->Obsoleted.hasValue()) {
          os << ",obsoleted=" << AvAttr->Obsoleted.getValue().getAsString();
        }
      }
      if (!AvAttr->Rename.empty() && isa<ValueDecl>(D)) {
        os << ",message=\"'" << cast<ValueDecl>(D)->getBaseName()
           << "' has been renamed to '";
        printRenameForDecl(AvAttr, cast<ValueDecl>(D), false);
        os << '\'';
        if (!AvAttr->Message.empty()) {
          os << ": ";
          printEncodedString(AvAttr->Message, false);
        }
        os << "\"";
      } else if (!AvAttr->Message.empty()) {
        os << ",message=";
        printEncodedString(AvAttr->Message);
      }
      os << ")";
    }
    return hasPrintedAnything;
  }
    
  void printRenameForDecl(const AvailableAttr *AvAttr, const ValueDecl *D,
                          bool includeQuotes) {
    assert(!AvAttr->Rename.empty());
    
    auto renamedParsedDeclName = parseDeclName(AvAttr->Rename);
    auto renamedDeclName = renamedParsedDeclName.formDeclName(D->getASTContext());
    
    auto declContext = D->getDeclContext();
    const ValueDecl *renamedDecl = nullptr;
    
    if (isa<ClassDecl>(D) || isa<ProtocolDecl>(D)) {
      UnqualifiedLookup lookup(renamedDeclName.getBaseIdentifier(),
                               declContext->getModuleScopeContext(),
                               nullptr,
                               SourceLoc(),
                               UnqualifiedLookup::Flags::TypeLookup);
      renamedDecl = lookup.getSingleTypeResult();
    } else {
      SmallVector<ValueDecl *, 4> lookupResults;
      declContext->lookupQualified(declContext->getSelfTypeInContext(),
                                   renamedDeclName, NL_QualifiedDefault, NULL,
                                   lookupResults);
      for (auto candidate : lookupResults) {
        if (!shouldInclude(candidate))
          continue;
        
        if (candidate->getKind() != D->getKind() ||
            (candidate->isInstanceMember() !=
             cast<ValueDecl>(D)->isInstanceMember()))
          continue;
        
        if (isa<FuncDecl>(candidate) &&
            (cast<FuncDecl>(candidate)->getParameters()->size() !=
             cast<FuncDecl>(D)->getParameters()->size()))
          continue;
        
        if (renamedDecl) {
          // If we found a duplicated candidate then we would silently fail.
          renamedDecl = nullptr;
          break;
        }
        renamedDecl = candidate;
      }
    }
    
    if (renamedDecl) {
      SmallString<128> scratch;
      auto renamedObjCRuntimeName = renamedDecl->getObjCRuntimeName()
        ->getString(scratch);
      printEncodedString(renamedObjCRuntimeName, includeQuotes);
    } else {
      printEncodedString(AvAttr->Rename, includeQuotes);
    }
  }

  void printSwift3ObjCDeprecatedInference(ValueDecl *VD) {
    const LangOptions &langOpts = M.getASTContext().LangOpts;
    if (!langOpts.EnableSwift3ObjCInference ||
        langOpts.WarnSwift3ObjCInference == Swift3ObjCInferenceWarnings::None) {
      return;
    }
    auto attr = VD->getAttrs().getAttribute<ObjCAttr>();
    if (!attr || !attr->isSwift3Inferred())
      return;

    os << " SWIFT_DEPRECATED_OBJC(\"Swift ";
    if (isa<VarDecl>(VD))
      os << "property";
    else if (isa<SubscriptDecl>(VD))
      os << "subscript";
    else if (isa<ConstructorDecl>(VD))
      os << "initializer";
    else
      os << "method";
    os << " '";
    auto nominal =
      VD->getDeclContext()->getAsNominalTypeOrNominalTypeExtensionContext();
    printEncodedString(nominal->getName().str(), /*includeQuotes=*/false);
    os << ".";
    SmallString<32> scratch;
    printEncodedString(VD->getFullName().getString(scratch),
                       /*includeQuotes=*/false);
    os << "' uses '@objc' inference deprecated in Swift 4; add '@objc' to "
       <<   "provide an Objective-C entrypoint\")";
  }

  void visitFuncDecl(FuncDecl *FD) {
    if (FD->getDeclContext()->isTypeContext())
      printAbstractFunctionAsMethod(FD, FD->isStatic());
    else
      printAbstractFunctionAsFunction(FD);
  }

  void visitConstructorDecl(ConstructorDecl *CD) {
    printAbstractFunctionAsMethod(CD, false);
  }

  bool maybePrintIBOutletCollection(Type ty) {
    if (auto unwrapped = ty->getOptionalObjectType())
      ty = unwrapped;

    auto genericTy = ty->getAs<BoundGenericStructType>();
    if (!genericTy || genericTy->getDecl() != M.getASTContext().getArrayDecl())
      return false;

    assert(genericTy->getGenericArgs().size() == 1);

    auto argTy = genericTy->getGenericArgs().front();
    if (auto classDecl = argTy->getClassOrBoundGenericClass())
      os << "IBOutletCollection(" << getNameForObjC(classDecl) << ") ";
    else
      os << "IBOutletCollection(id) ";
    return true;
  }

  bool isCFTypeRef(Type ty) {
    if (ID_CFTypeRef.empty())
      ID_CFTypeRef = M.getASTContext().getIdentifier("CFTypeRef");

    const TypeAliasDecl *TAD = nullptr;
    while (auto aliasTy = dyn_cast<NameAliasType>(ty.getPointer())) {
      TAD = aliasTy->getDecl();
      ty = aliasTy->getSinglyDesugaredType();
    }

    return TAD && TAD->getName() == ID_CFTypeRef && TAD->hasClangNode();
  }

  void visitVarDecl(VarDecl *VD) {
    assert(VD->getDeclContext()->isTypeContext() &&
           "cannot handle global variables right now");

    printDocumentationComment(VD);

    if (VD->isStatic()) {
      // Older Clangs don't support class properties.
      os << "SWIFT_CLASS_PROPERTY(";
    }

    // For now, never promise atomicity.
    os << "@property (nonatomic";

    if (VD->isStatic())
      os << ", class";

    ASTContext &ctx = M.getASTContext();
    bool isSettable = VD->isSettable(nullptr);
    if (isSettable && ctx.LangOpts.EnableAccessControl)
      isSettable = (VD->getSetterFormalAccess() >= minRequiredAccess);
    if (!isSettable)
      os << ", readonly";

    // Print the ownership semantics, if relevant.
    // We treat "unowned" as "assign" (even though it's more like
    // "safe_unretained") because we want people to think twice about
    // allowing that object to disappear.
    Type ty = VD->getInterfaceType();
    if (auto weakTy = ty->getAs<WeakStorageType>()) {
      auto innerTy = weakTy->getReferentType()->getOptionalObjectType();
      auto innerClass = innerTy->getClassOrBoundGenericClass();
      if ((innerClass &&
           innerClass->getForeignClassKind()!=ClassDecl::ForeignKind::CFType) ||
          (innerTy->isObjCExistentialType() && !isCFTypeRef(innerTy))) {
        os << ", weak";
      }
    } else if (ty->is<UnownedStorageType>()) {
      os << ", assign";
    } else if (ty->is<UnmanagedStorageType>()) {
      os << ", unsafe_unretained";
    } else {
      Type copyTy = ty;
      bool isOptional = false;
      if (auto unwrappedTy = copyTy->getOptionalObjectType()) {
        isOptional = true;
        copyTy = unwrappedTy;
      }

      auto nominal = copyTy->getNominalOrBoundGenericNominal();
      if (nominal && isa<StructDecl>(nominal)) {
        if (nominal == ctx.getArrayDecl() ||
            nominal == ctx.getDictionaryDecl() ||
            nominal == ctx.getSetDecl() ||
            nominal == ctx.getStringDecl() ||
            (!getKnownTypeInfo(nominal) && getObjCBridgedClass(nominal))) {
          // We fast-path the most common cases in the condition above.
          os << ", copy";
        } else if (nominal == ctx.getUnmanagedDecl()) {
          os << ", unsafe_unretained";
          // Don't print unsafe_unretained twice.
          if (auto boundTy = copyTy->getAs<BoundGenericType>()) {
            ty = boundTy->getGenericArgs().front();
            if (isOptional)
              ty = OptionalType::get(ty);
          }
        }
      } else if (auto fnTy = copyTy->getAs<FunctionType>()) {
        switch (fnTy->getRepresentation()) {
        case FunctionTypeRepresentation::Block:
        case FunctionTypeRepresentation::Swift:
          os << ", copy";
          break;
        case FunctionTypeRepresentation::Thin:
        case FunctionTypeRepresentation::CFunctionPointer:
          break;
        }
      } else if ((nominal && isa<ClassDecl>(nominal) &&
                  cast<ClassDecl>(nominal)->getForeignClassKind() !=
                    ClassDecl::ForeignKind::CFType) ||
                 (copyTy->isObjCExistentialType() && !isCFTypeRef(copyTy))) {
        os << ", strong";
      }
    }

    Identifier objCName = VD->getObjCPropertyName();
    bool hasReservedName = isClangKeyword(objCName);

    // Handle custom accessor names.
    llvm::SmallString<64> buffer;
    if (hasReservedName ||
        VD->getObjCGetterSelector() !=
          VarDecl::getDefaultObjCGetterSelector(ctx, objCName)) {
      os << ", getter=" << VD->getObjCGetterSelector().getString(buffer);
    }
    if (isSettable) {
      if (hasReservedName ||
          VD->getObjCSetterSelector() !=
            VarDecl::getDefaultObjCSetterSelector(ctx, objCName)) {
        buffer.clear();
        os << ", setter=" << VD->getObjCSetterSelector().getString(buffer);
      }
    }

    os << ") ";
    if (VD->getAttrs().hasAttribute<IBOutletAttr>()) {
      if (!maybePrintIBOutletCollection(ty))
        os << "IBOutlet ";
    }

    clang::QualType clangTy;
    if (const VarDecl *base = findClangBase(VD))
      if (auto prop = dyn_cast<clang::ObjCPropertyDecl>(base->getClangDecl()))
        clangTy = prop->getType();

    if (!clangTy.isNull() && isNSUInteger(clangTy)) {
      os << "NSUInteger " << objCName;
      if (hasReservedName)
        os << "_";
    } else {
      OptionalTypeKind kind;
      Type objTy;
      std::tie(objTy, kind) = getObjectTypeAndOptionality(VD, ty);
      print(objTy, kind, objCName);
    }

    printSwift3ObjCDeprecatedInference(VD);

    printAvailability(VD);

    os << ";";
    if (VD->isStatic()) {
      os << ")\n";
      // Older Clangs don't support class properties, so print the accessors as
      // well. This is harmless.
      printAbstractFunctionAsMethod(VD->getGetter(), true);
      if (isSettable) {
        assert(VD->getSetter() && "settable ObjC property missing setter decl");
        printAbstractFunctionAsMethod(VD->getSetter(), true);
      }
    } else {
      os << "\n";
      if (looksLikeInitMethod(VD->getObjCGetterSelector()))
        printAbstractFunctionAsMethod(VD->getGetter(), false);
      if (isSettable && looksLikeInitMethod(VD->getObjCSetterSelector()))
        printAbstractFunctionAsMethod(VD->getSetter(), false);
    }
  }

  void visitSubscriptDecl(SubscriptDecl *SD) {
    assert(SD->isInstanceMember() && "static subscripts not supported");

    bool isNSUIntegerSubscript = false;
    if (auto clangBase = findClangBase(SD)) {
      const auto *clangGetter =
          cast<clang::ObjCMethodDecl>(clangBase->getClangDecl());
      const auto *indexParam = clangGetter->parameters().front();
      isNSUIntegerSubscript = isNSUInteger(indexParam->getType());
    }

    printAbstractFunctionAsMethod(SD->getGetter(), false, isNSUIntegerSubscript);
    if (auto setter = SD->getSetter())
      printAbstractFunctionAsMethod(setter, false, isNSUIntegerSubscript);
  }

  /// Visit part of a type, such as the base of a pointer type.
  ///
  /// If a full type is being printed, use print() instead.
  void visitPart(Type ty, Optional<OptionalTypeKind> optionalKind) {
    TypeVisitor::visit(ty, optionalKind);
  }

  /// Where nullability information should be printed.
  enum class NullabilityPrintKind {
    Before,
    After,
    ContextSensitive,
  };

  void printNullability(Optional<OptionalTypeKind> kind,
                        NullabilityPrintKind printKind
                          = NullabilityPrintKind::After) {
    if (!kind)
      return;

    switch (printKind) {
    case NullabilityPrintKind::ContextSensitive:
      switch (*kind) {
      case OTK_None:
        os << "nonnull";
        break;
      case OTK_Optional:
        os << "nullable";
        break;
      case OTK_ImplicitlyUnwrappedOptional:
        os << "null_unspecified";
        break;
      }
      break;
    case NullabilityPrintKind::After:
      os << ' ';
      LLVM_FALLTHROUGH;
    case NullabilityPrintKind::Before:
      switch (*kind) {
      case OTK_None:
        os << "_Nonnull";
        break;
      case OTK_Optional:
        os << "_Nullable";
        break;
      case OTK_ImplicitlyUnwrappedOptional:
        os << "_Null_unspecified";
        break;
      }
      break;
    }

    if (printKind != NullabilityPrintKind::After)
      os << ' ';
  }

  /// Determine whether this generic Swift nominal type maps to a
  /// generic Objective-C class.
  static bool hasGenericObjCType(const NominalTypeDecl *nominal) {
    auto clangDecl = nominal->getClangDecl();
    if (!clangDecl) return false;

    auto objcClass = dyn_cast<clang::ObjCInterfaceDecl>(clangDecl);
    if (!objcClass) return false;

    if (objcClass->getTypeParamList() == nullptr) return false;

    return true;
  }

  /// If \p nominal is bridged to an Objective-C class (via a conformance to
  /// _ObjectiveCBridgeable), return that class.
  ///
  /// Otherwise returns null.
  const ClassDecl *getObjCBridgedClass(const NominalTypeDecl *nominal) {
    // Print imported bridgeable decls as their unbridged type.
    if (nominal->hasClangNode())
      return nullptr;

    auto &ctx = nominal->getASTContext();

    // Dig out the ObjectiveCBridgeable protocol.
    auto proto = ctx.getProtocol(KnownProtocolKind::ObjectiveCBridgeable);
    if (!proto) return nullptr;

    // Determine whether this nominal type is _ObjectiveCBridgeable.
    SmallVector<ProtocolConformance *, 2> conformances;
    if (!nominal->lookupConformance(&M, proto, conformances))
      return nullptr;

    // Dig out the Objective-C type.
    auto conformance = conformances.front();
    Type objcType = ProtocolConformanceRef::getTypeWitnessByName(
                      nominal->getDeclaredType(),
                      ProtocolConformanceRef(conformance),
                      ctx.Id_ObjectiveCType,
                      nullptr);
    if (!objcType) return nullptr;

    // Dig out the Objective-C class.
    return objcType->getClassOrBoundGenericClass();
  }

  /// If the nominal type is bridged to Objective-C (via a conformance
  /// to _ObjectiveCBridgeable), print the bridged type.
  void printObjCBridgeableType(const NominalTypeDecl *swiftNominal,
                               const ClassDecl *objcClass,
                               ArrayRef<Type> typeArgs,
                               Optional<OptionalTypeKind> optionalKind) {
    auto &ctx = swiftNominal->getASTContext();
    assert(objcClass);

    Type rewrittenArgsBuf[2];

    // Detect when the type arguments correspond to the unspecialized
    // type, and clear them out. There is some type-specific hackery
    // here for:
    //
    //   NSArray<id> --> NSArray
    //   NSDictionary<NSObject *, id> --> NSDictionary
    //   NSSet<id> --> NSSet
    if (!typeArgs.empty() &&
        (!hasGenericObjCType(objcClass)
         || (swiftNominal == ctx.getArrayDecl() &&
             isAnyObjectOrAny(typeArgs[0]))
         || (swiftNominal == ctx.getDictionaryDecl() &&
             isNSObjectOrAnyHashable(ctx, typeArgs[0]) &&
             isAnyObjectOrAny(typeArgs[1]))
         || (swiftNominal == ctx.getSetDecl() &&
             isNSObjectOrAnyHashable(ctx, typeArgs[0])))) {
      typeArgs = {};
    }
    
    // Use the proper upper id<NSCopying> bound for Dictionaries with
    // upper-bounded keys.
    else if (swiftNominal == ctx.getDictionaryDecl() &&
             isNSObjectOrAnyHashable(ctx, typeArgs[0])) {
      if (ModuleDecl *M = ctx.getLoadedModule(ctx.Id_Foundation)) {
        if (!NSCopyingType) {
          UnqualifiedLookup lookup(ctx.getIdentifier("NSCopying"), M, nullptr);
          auto type = lookup.getSingleTypeResult();
          if (type && isa<ProtocolDecl>(type)) {
            NSCopyingType = type->getDeclaredInterfaceType();
          } else {
            NSCopyingType = Type();
          }
        }
        if (*NSCopyingType) {
          rewrittenArgsBuf[0] = *NSCopyingType;
          rewrittenArgsBuf[1] = typeArgs[1];
          typeArgs = rewrittenArgsBuf;
        }
      }
    }

    // Print the class type.
    SmallString<32> objcNameScratch;
    os << objcClass->getObjCRuntimeName(objcNameScratch);

    // Print the type arguments, if present.
    if (!typeArgs.empty()) {
      os << "<";
        interleave(typeArgs,
                   [this](Type type) {
                     printCollectionElement(type);
                   },
                   [this] { os << ", "; });
      os << ">";
    }

    os << " *";
    printNullability(optionalKind);
  }

  /// If the nominal type is bridged to Objective-C (via a conformance to
  /// _ObjectiveCBridgeable), print the bridged type. Otherwise, nothing is
  /// printed.
  ///
  /// \returns true iff printed something.
  bool printIfObjCBridgeable(const NominalTypeDecl *nominal,
                             ArrayRef<Type> typeArgs,
                             Optional<OptionalTypeKind> optionalKind) {
    if (const ClassDecl *objcClass = getObjCBridgedClass(nominal)) {
      printObjCBridgeableType(nominal, objcClass, typeArgs, optionalKind);
      return true;
    }
    return false;
  }

  /// If \p typeDecl is one of the standard library types used to map in Clang
  /// primitives and basic types, return the address of the info in
  /// \c specialNames containing the Clang name and whether it can be optional.
  ///
  /// Returns null if the name is not one of these known types.
  const NameAndOptional *getKnownTypeInfo(const TypeDecl *typeDecl) {
    if (specialNames.empty()) {
      ASTContext &ctx = M.getASTContext();
#define MAP(SWIFT_NAME, CLANG_REPR, NEEDS_NULLABILITY)                       \
      specialNames[{ctx.StdlibModuleName, ctx.getIdentifier(#SWIFT_NAME)}] = \
        { CLANG_REPR, NEEDS_NULLABILITY}

      MAP(CBool, "bool", false);

      MAP(CChar, "char", false);
      MAP(CWideChar, "wchar_t", false);
      MAP(CChar16, "char16_t", false);
      MAP(CChar32, "char32_t", false);

      MAP(CSignedChar, "signed char", false);
      MAP(CShort, "short", false);
      MAP(CInt, "int", false);
      MAP(CLong, "long", false);
      MAP(CLongLong, "long long", false);

      MAP(CUnsignedChar, "unsigned char", false);
      MAP(CUnsignedShort, "unsigned short", false);
      MAP(CUnsignedInt, "unsigned int", false);
      MAP(CUnsignedLong, "unsigned long", false);
      MAP(CUnsignedLongLong, "unsigned long long", false);

      MAP(CFloat, "float", false);
      MAP(CDouble, "double", false);

      MAP(Int8, "int8_t", false);
      MAP(Int16, "int16_t", false);
      MAP(Int32, "int32_t", false);
      MAP(Int64, "int64_t", false);
      MAP(UInt8, "uint8_t", false);
      MAP(UInt16, "uint16_t", false);
      MAP(UInt32, "uint32_t", false);
      MAP(UInt64, "uint64_t", false);

      MAP(Float, "float", false);
      MAP(Double, "double", false);
      MAP(Float32, "float", false);
      MAP(Float64, "double", false);

      MAP(Int, "NSInteger", false);
      MAP(UInt, "NSUInteger", false);
      MAP(Bool, "BOOL", false);

      MAP(OpaquePointer, "void *", true);
      MAP(UnsafeRawPointer, "void const *", true);
      MAP(UnsafeMutableRawPointer, "void *", true);

      Identifier ID_ObjectiveC = ctx.Id_ObjectiveC;
      specialNames[{ID_ObjectiveC, ctx.getIdentifier("ObjCBool")}] 
        = { "BOOL", false};
      specialNames[{ID_ObjectiveC, ctx.getIdentifier("Selector")}] 
        = { "SEL", true };
      specialNames[{ID_ObjectiveC,
                    ctx.getIdentifier(
                      ctx.getSwiftName(KnownFoundationEntity::NSZone))}]
        = { "struct _NSZone *", true };

      specialNames[{ctx.Id_Darwin, ctx.getIdentifier("DarwinBoolean")}]
        = { "Boolean", false};

      Identifier ID_CoreGraphics = ctx.getIdentifier("CoreGraphics");
      specialNames[{ID_CoreGraphics, ctx.getIdentifier("CGFloat")}]
        = { "CGFloat", false };

      // Use typedefs we set up for SIMD vector types.
#define MAP_SIMD_TYPE(BASENAME, _, __) \
      specialNames[{ctx.Id_simd, ctx.getIdentifier(#BASENAME "2")}] \
        = { "swift_" #BASENAME "2", false };                        \
      specialNames[{ctx.Id_simd, ctx.getIdentifier(#BASENAME "3")}] \
        = { "swift_" #BASENAME "3", false };                        \
      specialNames[{ctx.Id_simd, ctx.getIdentifier(#BASENAME "4")}] \
        = { "swift_" #BASENAME "4", false };
#include "swift/ClangImporter/SIMDMappedTypes.def"
      static_assert(SWIFT_MAX_IMPORTED_SIMD_ELEMENTS == 4,
                    "must add or remove special name mappings if max number of "
                    "SIMD elements is changed");
    }

    Identifier moduleName = typeDecl->getModuleContext()->getName();
    Identifier name = typeDecl->getName();
    auto iter = specialNames.find({moduleName, name});
    if (iter == specialNames.end())
      return nullptr;
    return &iter->second;
  }

  /// If \p typeDecl is one of the standard library types used to map in Clang
  /// primitives and basic types, print out the appropriate spelling and
  /// return true.
  ///
  /// This handles typealiases and structs provided by the standard library
  /// for interfacing with C and Objective-C.
  bool printIfKnownSimpleType(const TypeDecl *typeDecl,
                              Optional<OptionalTypeKind> optionalKind) {
    auto *knownTypeInfo = getKnownTypeInfo(typeDecl);
    if (!knownTypeInfo)
      return false;
    os << knownTypeInfo->first;
    if (knownTypeInfo->second)
      printNullability(optionalKind);
    return true;
  }
  
  void visitType(TypeBase *Ty, Optional<OptionalTypeKind> optionalKind) {
    assert(Ty->getDesugaredType() == Ty && "unhandled sugared type");
    os << "/* ";
    Ty->print(os);
    os << " */";
  }

  bool isClangPointerType(const clang::TypeDecl *clangTypeDecl) const {
    ASTContext &ctx = M.getASTContext();
    auto &clangASTContext = ctx.getClangModuleLoader()->getClangASTContext();
    clang::QualType clangTy = clangASTContext.getTypeDeclType(clangTypeDecl);
    return clangTy->isPointerType() || clangTy->isBlockPointerType() ||
      clangTy->isObjCObjectPointerType();
  }

  bool printImportedAlias(const TypeAliasDecl *alias,
                          Optional<OptionalTypeKind> optionalKind) {
    if (!alias->hasClangNode()) {
      if (!alias->isObjC()) return false;

      os << alias->getName();
      return true;
    }

    if (auto *clangTypeDecl =
          dyn_cast<clang::TypeDecl>(alias->getClangDecl())) {
      maybePrintTagKeyword(alias);
      os << getNameForObjC(alias);

      if (isClangPointerType(clangTypeDecl))
        printNullability(optionalKind);
    } else if (auto *clangObjCClass
               = dyn_cast<clang::ObjCInterfaceDecl>(alias->getClangDecl())){
      os << clangObjCClass->getName() << " *";
      printNullability(optionalKind);
    } else {
      auto *clangCompatAlias =
      cast<clang::ObjCCompatibleAliasDecl>(alias->getClangDecl());
      os << clangCompatAlias->getName() << " *";
      printNullability(optionalKind);
    }

    return true;
  }

  void visitNameAliasType(NameAliasType *aliasTy,
                               Optional<OptionalTypeKind> optionalKind) {
    const TypeAliasDecl *alias = aliasTy->getDecl();
    if (printIfKnownSimpleType(alias, optionalKind))
      return;

    if (printImportedAlias(alias, optionalKind))
      return;

    visitPart(alias->getUnderlyingTypeLoc().getType(), optionalKind);
  }

  void maybePrintTagKeyword(const TypeDecl *NTD) {
    if (isa<EnumDecl>(NTD) && !NTD->hasClangNode()) {
      os << "enum ";
      return;
    }

    auto clangDecl = dyn_cast_or_null<clang::TagDecl>(NTD->getClangDecl());
    if (!clangDecl)
      return;

    if (clangDecl->getTypedefNameForAnonDecl())
      return;

    ASTContext &ctx = M.getASTContext();
    auto importer = static_cast<ClangImporter *>(ctx.getClangModuleLoader());
    if (importer->hasTypedef(clangDecl))
      return;

    os << clangDecl->getKindName() << " ";
  }

  void visitStructType(StructType *ST, 
                       Optional<OptionalTypeKind> optionalKind) {
    const StructDecl *SD = ST->getStructOrBoundGenericStruct();

    // Handle known type names.
    if (printIfKnownSimpleType(SD, optionalKind))
      return;

    // Handle bridged types.
    if (printIfObjCBridgeable(SD, { }, optionalKind))
      return;

    maybePrintTagKeyword(SD);
    os << getNameForObjC(SD);

    // Handle swift_newtype applied to a pointer type.
    if (auto *clangDecl = cast_or_null<clang::TypeDecl>(SD->getClangDecl()))
      if (isClangPointerType(clangDecl))
        printNullability(optionalKind);
  }

  /// Print a collection element type using Objective-C generics syntax.
  ///
  /// This will print the type as bridged to Objective-C.
  void printCollectionElement(Type ty) {
    ASTContext &ctx = M.getASTContext();

    auto isSwiftNewtype = [](const StructDecl *SD) -> bool {
      if (!SD)
        return false;
      auto *clangDecl = SD->getClangDecl();
      if (!clangDecl)
        return false;
      return clangDecl->hasAttr<clang::SwiftNewtypeAttr>();
    };

    // Use the type as bridged to Objective-C unless the element type is itself
    // an imported type or a collection.
    const StructDecl *SD = ty->getStructOrBoundGenericStruct();
    if (ty->isAny()) {
      ty = ctx.getAnyObjectType();
    } else if (SD != ctx.getArrayDecl() &&
        SD != ctx.getDictionaryDecl() &&
        SD != ctx.getSetDecl() &&
        !isSwiftNewtype(SD)) {
      ty = ctx.getBridgedToObjC(&M, ty);
    }
    
    assert(ty && "unknown bridged type");

    print(ty, None);
  }

  /// If \p BGT represents a generic struct used to import Clang types, print
  /// it out.
  bool printIfKnownGenericStruct(const BoundGenericStructType *BGT,
                                 Optional<OptionalTypeKind> optionalKind) {
    StructDecl *SD = BGT->getDecl();
    if (!SD->getModuleContext()->isStdlibModule())
      return false;

    ASTContext &ctx = M.getASTContext();

    if (SD == ctx.getUnmanagedDecl()) {
      auto args = BGT->getGenericArgs();
      assert(args.size() == 1);
      visitPart(args.front(), optionalKind);
      os << " __unsafe_unretained";
      return true;
    }

    // Everything from here on is some kind of pointer type.
    bool isConst;
    if (SD == ctx.getUnsafePointerDecl()) {
      isConst = true;
    } else if (SD == ctx.getAutoreleasingUnsafeMutablePointerDecl() ||
               SD == ctx.getUnsafeMutablePointerDecl()) {
      isConst = false;
    } else {
      // Not a pointer.
      return false;
    }

    auto args = BGT->getGenericArgs();
    assert(args.size() == 1);
    visitPart(args.front(), OTK_None);
    if (isConst)
      os << " const";
    os << " *";
    printNullability(optionalKind);
    return true;
  }

  void visitBoundGenericStructType(BoundGenericStructType *BGT,
                                   Optional<OptionalTypeKind> optionalKind) {
    // Handle bridged types.
    if (printIfObjCBridgeable(BGT->getDecl(), BGT->getGenericArgs(),
                              optionalKind))
      return;

    if (printIfKnownGenericStruct(BGT, optionalKind))
      return;

    visitBoundGenericType(BGT, optionalKind);
  }

  void printGenericArgs(BoundGenericType *BGT) {
    os << '<';
    interleave(BGT->getGenericArgs(),
               [this](Type t) { print(t, None); },
               [this] { os << ", "; });
    os << '>';
  }

  void visitBoundGenericClassType(BoundGenericClassType *BGT,
                                  Optional<OptionalTypeKind> optionalKind) {
    // Only handle imported ObjC generics.
    auto CD = BGT->getClassOrBoundGenericClass();
    if (!CD->isObjC())
      return visitType(BGT, optionalKind);
    
    assert(CD->getClangDecl() && "objc generic class w/o clang node?!");
    auto clangDecl = cast<clang::NamedDecl>(CD->getClangDecl());
    if (isa<clang::ObjCInterfaceDecl>(clangDecl)) {
      os << clangDecl->getName();
    } else {
      maybePrintTagKeyword(CD);
      os << clangDecl->getName();
    }
    printGenericArgs(BGT);
    if (isa<clang::ObjCInterfaceDecl>(clangDecl)) {
      os << " *";
    }
    printNullability(optionalKind);
  }

  void visitBoundGenericType(BoundGenericType *BGT,
                             Optional<OptionalTypeKind> optionalKind) {
    // Handle bridged types.
    if (!isa<StructDecl>(BGT->getDecl()) &&
        printIfObjCBridgeable(BGT->getDecl(), BGT->getGenericArgs(),
                              optionalKind))
      return;

    if (auto underlying = BGT->getOptionalObjectType()) {
      visitPart(underlying, OTK_Optional);
    } else
      visitType(BGT, optionalKind);
  }

  void visitEnumType(EnumType *ET, Optional<OptionalTypeKind> optionalKind) {
    const EnumDecl *ED = ET->getDecl();

    // Handle bridged types.
    if (printIfObjCBridgeable(ED, { }, optionalKind))
      return;

    maybePrintTagKeyword(ED);
    os << getNameForObjC(ED);
  }

  void visitClassType(ClassType *CT, Optional<OptionalTypeKind> optionalKind) {
    const ClassDecl *CD = CT->getClassOrBoundGenericClass();
    assert(CD->isObjC());
    auto clangDecl = dyn_cast_or_null<clang::NamedDecl>(CD->getClangDecl());
    if (clangDecl) {
      // Hack for <os/object.h> types, which use classes in Swift but
      // protocols in Objective-C, and a typedef to hide the difference.
      StringRef osObjectName = maybeGetOSObjectBaseName(clangDecl);
      if (!osObjectName.empty()) {
        os << osObjectName << "_t";
      } else if (isa<clang::ObjCInterfaceDecl>(clangDecl)) {
        os << clangDecl->getName() << " *";
      } else {
        maybePrintTagKeyword(CD);
        os << clangDecl->getName();
      }
    } else {
      os << getNameForObjC(CD) << " *";
    }
    printNullability(optionalKind);
  }

  void visitExistentialType(Type T,
                            Optional<OptionalTypeKind> optionalKind,
                            bool isMetatype = false) {
    auto layout = T->getExistentialLayout();
    if (layout.isErrorExistential()) {
      if (isMetatype) os << "Class";
      else os << "NSError *";
      printNullability(optionalKind);
      return;
    }

    if (auto superclass = layout.explicitSuperclass) {
      auto *CD = superclass->getClassOrBoundGenericClass();
      assert(CD->isObjC());
      if (isMetatype) {
        os << "SWIFT_METATYPE(" << getNameForObjC(CD) << ")";
      } else {
        os << getNameForObjC(CD);
        if (auto *BGT = superclass->getAs<BoundGenericClassType>())
          printGenericArgs(BGT);
      }
    } else {
      os << (isMetatype ? "Class" : "id");
    }

    SmallVector<ProtocolDecl *, 2> protos;
    for (auto proto : layout.getProtocols())
      protos.push_back(proto->getDecl());
    printProtocols(protos);

    if (layout.explicitSuperclass && !isMetatype)
      os << " *";

    printNullability(optionalKind);
  }

  void visitProtocolType(ProtocolType *PT,
                         Optional<OptionalTypeKind> optionalKind) {
    visitExistentialType(PT, optionalKind, /*isMetatype=*/false);
  }

  void visitProtocolCompositionType(ProtocolCompositionType *PCT, 
                                    Optional<OptionalTypeKind> optionalKind) {
    visitExistentialType(PCT, optionalKind, /*isMetatype=*/false);
  }

  void visitExistentialMetatypeType(ExistentialMetatypeType *MT,
                                    Optional<OptionalTypeKind> optionalKind) {
    Type instanceTy = MT->getInstanceType();
    visitExistentialType(instanceTy, optionalKind, /*isMetatype=*/true);
  }

  void visitMetatypeType(MetatypeType *MT, 
                         Optional<OptionalTypeKind> optionalKind) {
    Type instanceTy = MT->getInstanceType();
    if (auto classTy = instanceTy->getAs<ClassType>()) {
      const ClassDecl *CD = classTy->getDecl();
      assert(CD->isObjC());
      os << "SWIFT_METATYPE(" << getNameForObjC(CD) << ")";
      printNullability(optionalKind);
    } else {
      visitType(MT, optionalKind);
    }
  }

  void visitGenericTypeParamType(GenericTypeParamType *type,
                                 Optional<OptionalTypeKind> optionalKind) {
    const GenericTypeParamDecl *decl = type->getDecl();
    assert(decl && "can't print canonicalized GenericTypeParamType");

    if (auto *extension = dyn_cast<ExtensionDecl>(decl->getDeclContext())) {
      const ClassDecl *extendedClass =
          extension->getAsClassOrClassExtensionContext();
      assert(extendedClass->isGeneric());
      assert(extension->getGenericParams()->size() ==
             extendedClass->getGenericParams()->size() &&
             "extensions with custom generic parameters?");
      assert(extension->getGenericSignature()->getCanonicalSignature() ==
             extendedClass->getGenericSignature()->getCanonicalSignature() &&
             "constrained extensions or custom generic parameters?");
      type = extendedClass->getGenericEnvironment()->getSugaredType(type);
      decl = type->getDecl();
    }

    assert(decl->getClangDecl() && "can only handle imported ObjC generics");
    os << cast<clang::ObjCTypeParamDecl>(decl->getClangDecl())->getName();
    printNullability(optionalKind);
  }
                      
  void printFunctionType(FunctionType *FT, char pointerSigil,
                         Optional<OptionalTypeKind> optionalKind) {
    visitPart(FT->getResult(), OTK_None);
    os << " (" << pointerSigil;
    printNullability(optionalKind);
    openFunctionTypes.push_back(FT);
  }

  void visitFunctionType(FunctionType *FT, 
                         Optional<OptionalTypeKind> optionalKind) {
    switch (FT->getRepresentation()) {
    case AnyFunctionType::Representation::Thin:
      llvm_unreachable("can't represent thin functions in ObjC");
    // Native Swift function types bridge to block types.
    case AnyFunctionType::Representation::Swift:
    case AnyFunctionType::Representation::Block:
      printFunctionType(FT, '^', optionalKind);
      break;
    case AnyFunctionType::Representation::CFunctionPointer:
      printFunctionType(FT, '*', optionalKind);
    }
  }

  /// Print the part of a function type that appears after where the variable
  /// name would go.
  ///
  /// This is necessary to handle C's awful declarator syntax.
  /// "(A) -> ((B) -> C)" becomes "C (^ (^)(A))(B)".
  void finishFunctionType(const FunctionType *FT) {
    os << ")(";
    if (!FT->getParams().empty()) {
      interleave(FT->getParams(),
                 [this](const AnyFunctionType::Param &param) {
                   print(param.getType(), OTK_None, param.getLabel(),
                         IsFunctionParam);
                 },
                 [this] { os << ", "; });
    } else {
      os << "void";
    }
    os << ")";
  }

  void visitTupleType(TupleType *TT, Optional<OptionalTypeKind> optionalKind) {
    assert(TT->getNumElements() == 0);
    os << "void";
  }

  void visitParenType(ParenType *PT, Optional<OptionalTypeKind> optionalKind) {
    visitPart(PT->getSinglyDesugaredType(), optionalKind);
  }

  void visitSyntaxSugarType(SyntaxSugarType *SST, 
                            Optional<OptionalTypeKind> optionalKind) {
    visitPart(SST->getSinglyDesugaredType(), optionalKind);
  }

  void visitDynamicSelfType(DynamicSelfType *DST, 
                            Optional<OptionalTypeKind> optionalKind) {
    printNullability(optionalKind, NullabilityPrintKind::ContextSensitive);
    os << "instancetype";
  }

  void visitReferenceStorageType(ReferenceStorageType *RST, 
                                 Optional<OptionalTypeKind> optionalKind) {
    visitPart(RST->getReferentType(), optionalKind);
  }
  
  /// RAII class for printing multi-part C types, such as functions and arrays.
  class PrintMultiPartType {
    ObjCPrinter &Printer;
    decltype(ObjCPrinter::openFunctionTypes) savedFunctionTypes;
    
    PrintMultiPartType(const PrintMultiPartType &) = delete;
  public:
    PrintMultiPartType(ObjCPrinter &Printer)
      : Printer(Printer) {
      savedFunctionTypes.swap(Printer.openFunctionTypes);
    }
    
    void finish() {
      auto &openFunctionTypes = Printer.openFunctionTypes;
      while (!openFunctionTypes.empty()) {
        const FunctionType *openFunctionTy = openFunctionTypes.pop_back_val();
        Printer.finishFunctionType(openFunctionTy);
      }
      openFunctionTypes = std::move(savedFunctionTypes);
      savedFunctionTypes.clear();
    }
    
    ~PrintMultiPartType() {
      finish();
    }
  };

  /// Print a full type, optionally declaring the given \p name.
  ///
  /// This will properly handle nested function types (see
  /// finishFunctionType()). If only a part of a type is being printed, use
  /// visitPart().
public:
  void print(Type ty, Optional<OptionalTypeKind> optionalKind, 
             Identifier name = Identifier(),
             IsFunctionParam_t isFuncParam = IsNotFunctionParam) {
    PrettyStackTraceType trace(M.getASTContext(), "printing", ty);

    if (isFuncParam)
      if (auto fnTy = ty->lookThroughAllOptionalTypes()
                        ->getAs<AnyFunctionType>())
        if (fnTy->isNoEscape())
          os << "SWIFT_NOESCAPE ";

    PrintMultiPartType multiPart(*this);
    visitPart(ty, optionalKind);
    if (!name.empty()) {
      os << ' ' << name;
      if (isClangKeyword(name)) {
        os << '_';
      }
    }
  }
};

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

  void visitNameAliasType(NameAliasType *aliasTy) {
    Callback(*this, aliasTy->getDecl());
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
      visit(param.getType());
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
  ObjCPrinter printer;
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
      printer.print(ED->getRawType(), OTK_None);
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
          (void)addImport(TD);
          // Just in case, make sure the underlying type is visible too.
          finder.visit(TAD->getUnderlyingTypeLoc().getType());
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

    const ClassDecl *CD = ED->getExtendedType()->getClassOrBoundGenericClass();
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
           << "Domain = @\"" << M.getName() << "." << ED->getName() << "\";\n";
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

  static void getClangSubmoduleReversePath(SmallVectorImpl<StringRef> &path,
                                           const clang::Module *module) {
    // FIXME: This should be an API on clang::Module.
    do {
      path.push_back(module->Name);
      module = module->Parent;
    } while (module);
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

    SmallVector<StringRef, 8> leftReversePath;
    SmallVector<StringRef, 8> rightReversePath;
    getClangSubmoduleReversePath(leftReversePath, leftClangModule);
    getClangSubmoduleReversePath(rightReversePath, rightClangModule);

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
        SmallVector<StringRef, 8> submoduleNames;
        getClangSubmoduleReversePath(submoduleNames, clangModule);
        interleave(submoduleNames.rbegin(), submoduleNames.rend(),
                   [&out](StringRef next) { out << next; },
                   [&out] { out << "."; });
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
        auto baseClass = ED->getExtendedType()->getClassOrBoundGenericClass();
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
          auto baseClass = ED->getExtendedType()->getClassOrBoundGenericClass();
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
