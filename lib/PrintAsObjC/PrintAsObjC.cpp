//===-- PrintAsObjC.cpp - Emit a header file for a Swift AST --------------===//
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

#include "swift/PrintAsObjC/PrintAsObjC.h"
#include "swift/Strings.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/AST/Comment.h"
#include "swift/Basic/Version.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IDE/CommentConversion.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/Basic/Module.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

#ifndef SWIFT_DISABLE_OBJC_GENERICS
static bool isNSObject(Type type) {
  if (auto classDecl = type->getClassOrBoundGenericClass()) {
    return !classDecl->getName().empty() &&
           classDecl->getName().str() == "NSObject" &&
           !classDecl->getModuleContext()->Name.empty() &&
           classDecl->getModuleContext()->Name.str() == "ObjectiveC";
  }

  return false;
}
#endif

namespace {
class ObjCPrinter : private DeclVisitor<ObjCPrinter>,
                    private TypeVisitor<ObjCPrinter, void, 
                                        Optional<OptionalTypeKind>> {
  friend ASTVisitor;
  friend TypeVisitor;

  llvm::DenseMap<std::pair<Identifier, Identifier>, std::pair<StringRef, bool>>
    specialNames;
  Identifier ID_CFTypeRef;

  ASTContext &ctx;
  raw_ostream &os;

  SmallVector<const FunctionType *, 4> openFunctionTypes;

  Accessibility minRequiredAccess;
  bool protocolMembersOptional = false;

  friend ASTVisitor<ObjCPrinter>;
  friend TypeVisitor<ObjCPrinter>;

public:
  explicit ObjCPrinter(ASTContext &context, raw_ostream &out,
                       Accessibility access)
    : ctx(context), os(out), minRequiredAccess(access) {}

  void print(const Decl *D) {
    visit(const_cast<Decl *>(D));
  }

  bool shouldInclude(const ValueDecl *VD) {
    return VD->isObjC() && VD->getAccessibility() >= minRequiredAccess;
  }

private:
  using ASTVisitor::visit;

  /// Prints a protocol adoption list: <code>&lt;NSCoding, NSCopying&gt;</code>
  ///
  /// This method filters out non-ObjC protocols, along with the special
  /// AnyObject protocol.
  void printProtocols(ArrayRef<ProtocolDecl *> protos) {
    SmallVector<ProtocolDecl *, 4> protosToPrint;
    std::copy_if(protos.begin(), protos.end(),
                 std::back_inserter(protosToPrint),
                 [this](const ProtocolDecl *PD) -> bool {
      if (!shouldInclude(PD))
        return false;
      auto knownProtocol = PD->getKnownProtocolKind();
      if (!knownProtocol)
        return true;
      return *knownProtocol != KnownProtocolKind::AnyObject;
    });

    if (protosToPrint.empty())
      return;

    os << " <";
    interleave(protosToPrint,
               [this](const ProtocolDecl *PD) {
                 if (PD->hasClangNode()) {
                   SmallString<64> buf;
                   os << PD->getObjCRuntimeName(buf);
                 } else {
                   os << PD->getName().str();
                  }
               },
               [this] { os << ", "; });
    os << ">";
  }

  /// Prints the members of a class, extension, or protocol.
  void printMembers(DeclRange members) {
    for (auto member : members) {
      auto VD = dyn_cast<ValueDecl>(member);
      if (!VD || !shouldInclude(VD) || isa<TypeDecl>(VD))
        continue;
      if (auto FD = dyn_cast<FuncDecl>(VD))
        if (FD->isAccessor())
          continue;
      if (VD->getAttrs().hasAttribute<OptionalAttr>() != protocolMembersOptional) {
        protocolMembersOptional = VD->getAttrs().hasAttribute<OptionalAttr>();
        os << (protocolMembersOptional ? "@optional\n" : "@required\n");
      }
      visit(VD);
    }
  }

  void printDocumentationComment(Decl *D) {
    CommentContext TheCommentContext;
    if (auto *FC = getFullComment(TheCommentContext, D))
      ide::getDocumentationCommentAsDoxygen(TheCommentContext, FC, os);
  }

  void visitClassDecl(ClassDecl *CD) {
    printDocumentationComment(CD);
    llvm::SmallString<32> scratch;
    os << "SWIFT_CLASS(\"" << CD->getObjCRuntimeName(scratch) << "\")\n"
       << "@interface " << CD->getName();
    if (Type superTy = CD->getSuperclass())
      os << " : " << superTy->getClassOrBoundGenericClass()->getName();
    printProtocols(CD->getProtocols());
    os << "\n";
    printMembers(CD->getMembers());
    os << "@end\n";
  }

  void visitExtensionDecl(ExtensionDecl *ED) {
    auto baseClass = ED->getExtendedType()->getClassOrBoundGenericClass();
    os << "@interface " << baseClass->getName()
       << " (SWIFT_EXTENSION(" << ED->getModuleContext()->Name << "))";
    printProtocols(ED->getProtocols());
    os << "\n";
    printMembers(ED->getMembers());
    os << "@end\n";
  }

  void visitProtocolDecl(ProtocolDecl *PD) {
    printDocumentationComment(PD);
    llvm::SmallString<32> scratch;
    os << "SWIFT_PROTOCOL(\"" << PD->getObjCRuntimeName(scratch) << "\")\n"
       << "@protocol " << PD->getName();
    printProtocols(PD->getProtocols());
    os << "\n";
    assert(!protocolMembersOptional && "protocols start required");
    printMembers(PD->getMembers());
    protocolMembersOptional = false;
    os << "@end\n";
  }
  
  void visitEnumDecl(EnumDecl *ED) {
    printDocumentationComment(ED);
    llvm::SmallString<32> scratch;
    os << "typedef SWIFT_ENUM(";
    print(ED->getRawType(), OTK_None);
    os << ", " << ED->getName() << ") {\n";
    for (auto Elt : ED->getAllElements()) {
      printDocumentationComment(Elt);

      // Print the cases as the concatenation of the enum name with the case
      // name.
      os << "  " << ED->getName() << Elt->getName();
      
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

  StringRef printSingleMethodParam(StringRef selectorString,
                                   const Pattern *param) {
    StringRef firstPiece, restOfSelector;
    std::tie(firstPiece, restOfSelector) = selectorString.split(':');
    os << firstPiece << ":(";
    this->print(param->getType(), OTK_None);
    os << ")";

    if (isa<AnyPattern>(param))
      os << "_";
    else
      os << cast<NamedPattern>(param)->getBodyName();

    return restOfSelector;
  }

  void printAbstractFunction(AbstractFunctionDecl *AFD, bool isClassMethod) {
    printDocumentationComment(AFD);
    if (isClassMethod)
      os << "+ (";
    else
      os << "- (";

    Type rawMethodTy = AFD->getType()->castTo<AnyFunctionType>()->getResult();
    auto methodTy = rawMethodTy->castTo<FunctionType>();

    // Constructors and methods returning DynamicSelf return
    // instancetype.
    if (isa<ConstructorDecl>(AFD) ||
        (isa<FuncDecl>(AFD) && cast<FuncDecl>(AFD)->hasDynamicSelf())) {
      if (auto ctor = dyn_cast<ConstructorDecl>(AFD)) {
        printNullability(ctor->getFailability(),
                         NullabilityPrintKind::ContextSensitive);
      } else {
        auto func = cast<FuncDecl>(AFD);
        OptionalTypeKind optionalKind;
        (void)func->getResultType()->getAnyOptionalObjectType(optionalKind);
        printNullability(optionalKind,
                         NullabilityPrintKind::ContextSensitive);
      }

      os << "instancetype";
    } else if (methodTy->getResult()->isVoid() &&
               AFD->getAttrs().hasAttribute<IBActionAttr>()) {
      os << "IBAction";
    } else {
      print(methodTy->getResult(), OTK_None);
    }

    os << ")";

    auto bodyPatterns = AFD->getBodyParamPatterns();
    assert(bodyPatterns.size() == 2 && "not an ObjC-compatible method");

    llvm::SmallString<128> selectorBuf;
    StringRef selectorString = AFD->getObjCSelector().getString(selectorBuf);

    if (isa<ParenPattern>(bodyPatterns.back())) {
      // One argument.
      auto bodyPattern = bodyPatterns.back()->getSemanticsProvidingPattern();
      selectorString = printSingleMethodParam(selectorString, bodyPattern);

    } else {
      const TuplePattern *bodyParams = cast<TuplePattern>(bodyPatterns.back());
      if (bodyParams->getNumFields() == 0) {
        // Zero arguments.
        os << selectorString;
        selectorString = "";
      } else {
        // Two or more arguments, or one argument with name and type.
        interleave(bodyParams->getFields(),
                   [this, &selectorString] (const TuplePatternElt &param) {
                     auto pattern = param.getPattern();
                     pattern = pattern->getSemanticsProvidingPattern();
                     selectorString = printSingleMethodParam(selectorString,
                                                             pattern);
                   },
                   [this] { os << " "; });
      }
    }
    assert(selectorString.empty());

    // Swift designated initializers are Objective-C designated initializers.
    if (auto ctor = dyn_cast<ConstructorDecl>(AFD)) {
      if (ctor->isDesignatedInit() &&
          !isa<ProtocolDecl>(ctor->getDeclContext())) {
        os << " OBJC_DESIGNATED_INITIALIZER";
      }
    }

    os << ";\n";
  }

  void visitFuncDecl(FuncDecl *FD) {
    assert(FD->getDeclContext()->isTypeContext() &&
           "cannot handle free functions right now");
    printAbstractFunction(FD, FD->isStatic());
  }

  void visitConstructorDecl(ConstructorDecl *CD) {
    printAbstractFunction(CD, false);
  }

  bool maybePrintIBOutletCollection(Type ty) {
    if (auto unwrapped = ty->getAnyOptionalObjectType())
      ty = unwrapped;

    auto genericTy = ty->getAs<BoundGenericStructType>();
    if (!genericTy || genericTy->getDecl() != ctx.getArrayDecl())
      return false;

    assert(genericTy->getGenericArgs().size() == 1);

    auto argTy = genericTy->getGenericArgs().front();
    if (auto classDecl = argTy->getClassOrBoundGenericClass())
      os << "IBOutletCollection(" << classDecl->getName() << ") ";
    else
      os << "IBOutletCollection(id) ";
    return true;
  }

  bool isCFTypeRef(Type ty) {
    if (ID_CFTypeRef.empty())
      ID_CFTypeRef = ctx.getIdentifier("CFTypeRef");
    while (auto aliasTy = dyn_cast<NameAliasType>(ty.getPointer())) {
      const TypeAliasDecl *TAD = aliasTy->getDecl();
      if (TAD->hasClangNode() && TAD->getName() == ID_CFTypeRef)
        return true;
    }
    return false;
  }

  void visitVarDecl(VarDecl *VD) {
    assert(VD->getDeclContext()->isTypeContext() &&
           "cannot handle global variables right now");

    printDocumentationComment(VD);

    if (VD->isStatic()) {
      // Objective-C doesn't have class properties. Just print the accessors.
      printAbstractFunction(VD->getGetter(), true);
      if (auto setter = VD->getSetter())
        printAbstractFunction(setter, true);
      return;
    }

    // For now, never promise atomicity.
    os << "@property (nonatomic";

    bool isSettable = VD->isSettable(nullptr);
    if (isSettable && ctx.LangOpts.EnableAccessControl)
      isSettable = (VD->getSetterAccessibility() >= minRequiredAccess);
    if (!isSettable)
      os << ", readonly";

    // Print the ownership semantics, if relevant.
    // We treat "unowned" as "assign" (even though it's more like
    // "safe_unretained") because we want people to think twice about
    // allowing that object to disappear.
    // FIXME: Handle the "Unmanaged" wrapper struct.
    Type ty = VD->getType();
    if (auto weakTy = ty->getAs<WeakStorageType>()) {
      auto innerTy = weakTy->getReferentType()->getAnyOptionalObjectType();
      auto innerClass = innerTy->getClassOrBoundGenericClass();
      if ((innerClass && !innerClass->isForeign()) ||
          (innerTy->isObjCExistentialType() && !isCFTypeRef(innerTy))) {
        os << ", weak";
      }
    } else if (ty->is<UnownedStorageType>()) {
      os << ", assign";
    } else if (ty->is<UnmanagedStorageType>()) {
      os << ", unsafe_unretained";
    } else {
      Type copyTy = ty;
      if (auto unwrappedTy = copyTy->getAnyOptionalObjectType())
        copyTy = unwrappedTy;
      if (auto nominal = copyTy->getStructOrBoundGenericStruct()) {
        if (nominal == ctx.getArrayDecl() ||
            nominal == ctx.getDictionaryDecl() ||
            nominal == ctx.getSetDecl() ||
            nominal == ctx.getStringDecl()) {
          os << ", copy";
        }
      } else if (copyTy->is<FunctionType>()) {
        os << ", copy";
      }
    }

    // Even though Swift doesn't use custom accessor names, we need to be
    // consistent when an Objective-C property is overridden.
    // FIXME: Will we ever need to do this for properties that /don't/ come
    // from Objective-C?
    bool overridesObjC = false;
    for (VarDecl *baseDecl = VD->getOverriddenDecl(); baseDecl;
         baseDecl = baseDecl->getOverriddenDecl()) {
      if (baseDecl->hasClangNode()) {
        overridesObjC = true;
        break;
      }
    }

    if (overridesObjC) {
      llvm::SmallString<64> buffer;
      os << ", getter=" << VD->getObjCGetterSelector().getString(buffer);
      if (VD->isSettable(nullptr)) {
        buffer.clear();
        os << ", setter=" << VD->getObjCSetterSelector().getString(buffer);
      }
    }

    os << ") ";
    if (VD->getAttrs().hasAttribute<IBOutletAttr>()) {
      if (!maybePrintIBOutletCollection(ty))
        os << "IBOutlet ";
    }
    print(ty, OTK_None, VD->getName().str());
    os << ";\n";
  }

  void visitSubscriptDecl(SubscriptDecl *SD) {
    assert(SD->isInstanceMember() && "static subscripts not supported");
    printAbstractFunction(SD->getGetter(), false);
    if (auto setter = SD->getSetter())
      printAbstractFunction(setter, false);
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

    if (printKind == NullabilityPrintKind::After)
      os << ' ';

    switch (*kind) {
    case OTK_None:
      if (printKind == NullabilityPrintKind::ContextSensitive)
        os << "SWIFT_NULLABILITY(nonnull)";
      else
        os << "__nonnull";
      break;

    case OTK_Optional:
      if (printKind == NullabilityPrintKind::ContextSensitive)
        os << "SWIFT_NULLABILITY(nullable)";
      else
        os << "__nullable";
      break;

    case OTK_ImplicitlyUnwrappedOptional:
      if (printKind == NullabilityPrintKind::ContextSensitive)
        os << "SWIFT_NULLABILITY(null_unspecified)";
      else
        os << "__null_unspecified";
      break;
    }

    if (printKind != NullabilityPrintKind::After)
      os << ' ';
  }

  /// If "name" is one of the standard library types used to map in Clang
  /// primitives and basic types, print out the appropriate spelling and
  /// return true.
  ///
  /// This handles typealiases and structs provided by the standard library
  /// for interfacing with C and Objective-C.
  bool printIfKnownTypeName(Identifier moduleName, Identifier name,
                            Optional<OptionalTypeKind> optionalKind) {
    if (specialNames.empty()) {
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
      MAP(String, "NSString *", true);

      MAP(COpaquePointer, "void *", false);
      MAP(CMutableVoidPointer, "void *", false);
      MAP(CConstVoidPointer, "void const *", false);

      Identifier ID_ObjectiveC = ctx.getIdentifier(OBJC_MODULE_NAME);
      specialNames[{ID_ObjectiveC, ctx.getIdentifier("ObjCBool")}] 
        = { "BOOL", false};
      specialNames[{ID_ObjectiveC, ctx.getIdentifier("Selector")}] 
        = { "SEL", false };
      specialNames[{ID_ObjectiveC, ctx.getIdentifier("NSZone")}] 
        = { "NSZone *", false };
    }

    auto iter = specialNames.find({moduleName, name});
    if (iter == specialNames.end())
      return false;

    os << iter->second.first;
    if (iter->second.second)
      printNullability(optionalKind);
    return true;
  }

  void visitType(TypeBase *Ty, Optional<OptionalTypeKind> optionalKind) {
    assert(Ty->getDesugaredType() == Ty && "unhandled sugared type");
    os << "/* ";
    Ty->print(os);
    os << " */";
  }

  void visitNameAliasType(NameAliasType *aliasTy,
                          Optional<OptionalTypeKind> optionalKind) {
    const TypeAliasDecl *alias = aliasTy->getDecl();
    if (printIfKnownTypeName(alias->getModuleContext()->Name, alias->getName(),
                             optionalKind))
      return;

    if (alias->hasClangNode()) {
      os << cast<clang::NamedDecl>(alias->getClangDecl())->getName();
      return;
    }

    if (alias->isObjC()) {
      os << alias->getName();
      return;
    }

    visitPart(alias->getUnderlyingType(), optionalKind);
  }

  void maybePrintTagKeyword(const NominalTypeDecl *NTD) {
    auto clangDecl = dyn_cast_or_null<clang::TagDecl>(NTD->getClangDecl());
    if (!clangDecl)
      return;

    if (clangDecl->getTypedefNameForAnonDecl())
      return;

    auto importer = static_cast<ClangImporter *>(ctx.getClangModuleLoader());
    if (importer->hasTypedef(clangDecl))
      return;

    os << clangDecl->getKindName() << " ";
  }

  void visitStructType(StructType *ST, 
                       Optional<OptionalTypeKind> optionalKind) {
    const StructDecl *SD = ST->getStructOrBoundGenericStruct();
    if (printIfKnownTypeName(SD->getModuleContext()->Name, SD->getName(),
                             optionalKind))
      return;

    maybePrintTagKeyword(SD);
    os << SD->getName();
  }

  /// If \p BGT represents a generic struct used to import Clang types, print
  /// it out.
  bool printIfKnownGenericStruct(const BoundGenericStructType *BGT,
                                 Optional<OptionalTypeKind> optionalKind) {
    StructDecl *SD = BGT->getDecl();
    if (!SD->getModuleContext()->isStdlibModule())
      return false;

    if (SD == ctx.getArrayDecl()) {
#ifndef SWIFT_DISABLE_OBJC_GENERICS
      if (!BGT->getGenericArgs()[0]->isAnyObject()) {
        os << "NS_ARRAY(";
        visitPart(BGT->getGenericArgs()[0], None);
        os << ")";
      } else {
        os << "NSArray *";
      }
#else
      os << "NSArray *";
#endif

      printNullability(optionalKind);
      return true;
    }

    if (SD == ctx.getDictionaryDecl()) {
#ifndef SWIFT_DISABLE_OBJC_GENERICS
      if (!isNSObject(BGT->getGenericArgs()[0]) ||
          !BGT->getGenericArgs()[1]->isAnyObject()) {
        os << "NS_DICTIONARY(";
        visitPart(BGT->getGenericArgs()[0], None);
        os << ", ";
        visitPart(BGT->getGenericArgs()[1], None);
        os << ")";
      } else {
        os << "NSDictionary *";
      }
#else
      os << "NSDictionary *";
#endif

      printNullability(optionalKind);
      return true;
    }

    if (SD == ctx.getSetDecl()) {
#ifndef SWIFT_DISABLE_OBJC_GENERICS
      if (!isNSObject(BGT->getGenericArgs()[0])) {
        os << "NS_SET(";
        visitPart(BGT->getGenericArgs()[0], None);
        os << ")";
      } else {
        os << "NSSet *";
      }
#else
      os << "NSSet *";
#endif

      printNullability(optionalKind);
      return true;
    }

    if (SD == ctx.getCFunctionPointerDecl()) {
      assert(BGT->getGenericArgs().size() == 1);
      auto FT = BGT->getGenericArgs()[0]->castTo<FunctionType>();
      printFunctionType(FT, '*', OTK_ImplicitlyUnwrappedOptional);
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
    return true;
  }

  void visitBoundGenericStructType(BoundGenericStructType *BGT,
                                   Optional<OptionalTypeKind> optionalKind) {
    if (printIfKnownGenericStruct(BGT, optionalKind))
      return;
    visitBoundGenericType(BGT, optionalKind);
  }

  void visitBoundGenericType(BoundGenericType *BGT,
                             Optional<OptionalTypeKind> optionalKind) {
    OptionalTypeKind innerOptionalKind;
    if (auto underlying = BGT->getAnyOptionalObjectType(innerOptionalKind)) {
      visitPart(underlying, innerOptionalKind);
    } else
      visitType(BGT, optionalKind);
  }

  void visitEnumType(EnumType *ET, Optional<OptionalTypeKind> optionalKind) {
    const EnumDecl *ED = ET->getDecl();
    maybePrintTagKeyword(ED);
    
    // Use the "enum" introducer to safely reference forward declarations.
    os << "enum " << ED->getName();
  }

  void visitClassType(ClassType *CT, Optional<OptionalTypeKind> optionalKind) {
    const ClassDecl *CD = CT->getClassOrBoundGenericClass();
    assert(CD->isObjC());
    auto clangDecl = dyn_cast_or_null<clang::NamedDecl>(CD->getClangDecl());
    if (clangDecl) {
      if (isa<clang::ObjCInterfaceDecl>(clangDecl)) {
        os << clangDecl->getName() << " *";
        printNullability(optionalKind);
      } else {
        maybePrintTagKeyword(CD);
        os << clangDecl->getName();
      }
    } else {
      os << CD->getName() << " *";
      printNullability(optionalKind);
    }
  }

  void visitProtocolType(ProtocolType *PT, 
                         Optional<OptionalTypeKind> optionalKind, 
                         bool isMetatype = false) {
    os << (isMetatype ? "Class" : "id");

    auto proto = PT->getDecl();
    assert(proto->isObjC());
    if (auto knownKind = proto->getKnownProtocolKind()) {
      if (*knownKind == KnownProtocolKind::AnyObject) {
        printNullability(optionalKind);
        return;
      }
    }

    printProtocols(proto);
    printNullability(optionalKind);
  }

  void visitProtocolCompositionType(ProtocolCompositionType *PCT, 
                                    Optional<OptionalTypeKind> optionalKind,
                                    bool isMetatype = false) {
    CanType canonicalComposition = PCT->getCanonicalType();
    if (auto singleProto = dyn_cast<ProtocolType>(canonicalComposition))
      return visitProtocolType(singleProto, optionalKind, isMetatype);
    PCT = cast<ProtocolCompositionType>(canonicalComposition);

    os << (isMetatype ? "Class" : "id");

    SmallVector<ProtocolDecl *, 4> protos;
    std::transform(PCT->getProtocols().begin(), PCT->getProtocols().end(),
                   std::back_inserter(protos),
                   [] (Type ty) -> ProtocolDecl * {
      return ty->castTo<ProtocolType>()->getDecl();
    });
    printProtocols(protos);
    printNullability(optionalKind);
  }

  void visitExistentialMetatypeType(ExistentialMetatypeType *MT, 
                                    Optional<OptionalTypeKind> optionalKind) {
    Type instanceTy = MT->getInstanceType();
    if (auto protoTy = instanceTy->getAs<ProtocolType>()) {
      visitProtocolType(protoTy, optionalKind, /*isMetatype=*/true);
    } else if (auto compTy = instanceTy->getAs<ProtocolCompositionType>()) {
      visitProtocolCompositionType(compTy, optionalKind, /*isMetatype=*/true);
    } else {
      visitType(MT, optionalKind);
    }
  }

  void visitMetatypeType(MetatypeType *MT, 
                         Optional<OptionalTypeKind> optionalKind) {
    Type instanceTy = MT->getInstanceType();
    if (auto classTy = instanceTy->getAs<ClassType>()) {
      const ClassDecl *CD = classTy->getDecl();
      if (CD->isObjC())
        os << "SWIFT_METATYPE(" << CD->getName() << ")";
      else
        os << "Class";
      printNullability(optionalKind);
    } else {
      visitType(MT, optionalKind);
    }
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
      llvm_unreachable("can't handle thin functions yet");
    // Native Swift function types bridge to block types.
    case AnyFunctionType::Representation::Thick:
    case AnyFunctionType::Representation::Block:
      printFunctionType(FT, '^', optionalKind);
      break;
    }
  }

  /// Print the part of a function type that appears after where the variable
  /// name would go.
  ///
  /// This is necessary to handle C's awful declarator syntax.
  /// "(A) -> ((B) -> C)" becomes "C (^ (^)(A))(B)".
  void finishFunctionType(const FunctionType *FT) {
    os << ")(";
    Type paramsTy = FT->getInput();
    if (auto tupleTy = dyn_cast<TupleType>(paramsTy.getPointer())) {
      if (tupleTy->getNumElements() == 0) {
        os << "void";
      } else {
        interleave(tupleTy->getElementTypes(),
                   [this](Type ty) { print(ty, OTK_None); },
                   [this] { os << ", "; });
      }
    } else {
      print(paramsTy, OTK_None);
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

  void visitSubstitutedType(SubstitutedType *ST, 
                            Optional<OptionalTypeKind> optionalKind) {
    visitPart(ST->getSinglyDesugaredType(), optionalKind);
  }

  void visitSyntaxSugarType(SyntaxSugarType *SST, 
                            Optional<OptionalTypeKind> optionalKind) {
    visitPart(SST->getSinglyDesugaredType(), optionalKind);
  }

  void visitDictionaryType(DictionaryType *DT, 
                           Optional<OptionalTypeKind> optionalKind) {
    visitPart(DT->getSinglyDesugaredType(), optionalKind);
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

  /// Print a full type, optionally declaring the given \p name.
  ///
  /// This will properly handle nested function types (see
  /// finishFunctionType()). If only a part of a type is being printed, use
  /// visitPart().
public:
  void print(Type ty, Optional<OptionalTypeKind> optionalKind, 
             StringRef name = "") {
    decltype(openFunctionTypes) savedFunctionTypes;
    savedFunctionTypes.swap(openFunctionTypes);

    visitPart(ty, optionalKind);
    if (!name.empty())
      os << ' ' << name;
    while (!openFunctionTypes.empty()) {
      const FunctionType *openFunctionTy = openFunctionTypes.pop_back_val();
      finishFunctionType(openFunctionTy);
    }

    openFunctionTypes = std::move(savedFunctionTypes);
  }
};

class ReferencedTypeFinder : private TypeVisitor<ReferencedTypeFinder> {
  friend TypeVisitor;

  llvm::function_ref<void(ReferencedTypeFinder &, const TypeDecl *)> Callback;

  ReferencedTypeFinder(decltype(Callback) callback) : Callback(callback) {}

  void visitType(TypeBase *base) {
    assert(base->getDesugaredType() == base && "unhandled sugared type");
    return;
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

  void visitNominalType(NominalType *nominal) {
    Callback(*this, nominal->getDecl());
  }

  void visitMetatypeType(MetatypeType *metatype) {
    visit(metatype->getInstanceType());
  }

  void visitSubstitutedType(SubstitutedType *sub) {
    visit(sub->getSinglyDesugaredType());
  }

  void visitAnyFunctionType(AnyFunctionType *fnTy) {
    visit(fnTy->getInput());
    visit(fnTy->getResult());
  }

  void visitSyntaxSugarType(SyntaxSugarType *sugar) {
    visit(sugar->getSinglyDesugaredType());
  }

  void visitDictionaryType(DictionaryType *DT) {
    visit(DT->getSinglyDesugaredType());
  }

  void visitProtocolCompositionType(ProtocolCompositionType *composition) {
    for (auto proto : composition->getProtocols())
      visit(proto);
  }

  void visitLValueType(LValueType *lvalue) {
    visit(lvalue->getObjectType());
  }

  void visitInOutType(InOutType *inout) {
    visit(inout->getObjectType());
  }

  void visitBoundGenericType(BoundGenericType *boundGeneric) {
    for (auto argTy : boundGeneric->getGenericArgs())
      visit(argTy);
    // Ignore the base type; that can't be exposed to Objective-C. Every
    // bound generic type we care about gets mapped to a particular construct
    // in Objective-C we care about. (For example, Optional<NSFoo> is mapped to
    // NSFoo *.)
  }

public:
  using TypeVisitor::visit;

  static void walk(Type ty, decltype(Callback) callback) {
    ReferencedTypeFinder(callback).visit(ty);
  }
};

/// A generalization of llvm::SmallSetVector that allows a custom comparator.
template <typename T, unsigned N, typename C = std::less<T>>
using SmallSetVector =
  llvm::SetVector<T, SmallVector<T, N>, llvm::SmallSet<T, N, C>>;

/// A comparator for types with PointerLikeTypeTraits that sorts by opaque
/// void pointer representation.
template <typename T>
struct PointerLikeComparator {
  using Traits = llvm::PointerLikeTypeTraits<T>;
  bool operator()(T lhs, T rhs) {
    return std::less<void*>()(Traits::getAsVoidPointer(lhs),
                              Traits::getAsVoidPointer(rhs));
  }
};

class ModuleWriter {
  enum class EmissionState {
    DefinitionRequested = 0,
    DefinitionInProgress,
    Defined
  };

  llvm::DenseMap<const TypeDecl *, std::pair<EmissionState, bool>> seenTypes;
  std::vector<const Decl *> declsToWrite;

  using ImportModuleTy = PointerUnion<Module*, const clang::Module*>;
  SmallSetVector<ImportModuleTy, 8,
                 PointerLikeComparator<ImportModuleTy>> imports;

  std::string bodyBuffer;
  llvm::raw_string_ostream os{bodyBuffer};

  Module &M;
  StringRef bridgingHeader;
  ObjCPrinter printer;
public:
  ModuleWriter(Module &mod, StringRef header, Accessibility access)
    : M(mod), bridgingHeader(header), printer(M.Ctx, os, access) {}

  /// Returns true if we added the decl's module to the import set, false if
  /// the decl is a local decl.
  ///
  /// The standard library is special-cased: we assume that any types from it
  /// will be handled explicitly rather than needing an explicit @import.
  bool addImport(const Decl *D) {
    Module *otherModule = D->getModuleContext();

    if (otherModule == &M)
      return false;
    if (otherModule->isStdlibModule())
      return true;

    // If there's a Clang node, see if it comes from an explicit submodule.
    // Import that instead, looking through any implicit submodules.
    if (auto clangNode = D->getClangNode()) {
      auto importer =
        static_cast<ClangImporter *>(M.Ctx.getClangModuleLoader());
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

  bool require(const TypeDecl *D) {
    if (addImport(D)) {
      seenTypes[D] = { EmissionState::Defined, true };
      return true;
    }

    auto &state = seenTypes[D];
    switch (state.first) {
    case EmissionState::DefinitionRequested:
      declsToWrite.push_back(D);
      return false;
    case EmissionState::DefinitionInProgress:
      llvm_unreachable("circular requirements");
    case EmissionState::Defined:
      return true;
    }
  }

  void forwardDeclare(const NominalTypeDecl *NTD,
                      std::function<void (void)> Printer) {
    if (NTD->getModuleContext()->isStdlibModule())
      return;
    auto &state = seenTypes[NTD];
    if (state.second)
      return;
    Printer();
    state.second = true;
  }

  bool forwardDeclare(const ClassDecl *CD) {
    if (!CD->isObjC() || CD->isForeign())
      return false;
    forwardDeclare(CD, [&]{ os << "@class " << CD->getName() << ";\n"; });
    return true;
  }

  void forwardDeclare(const ProtocolDecl *PD) {
    assert(PD->isObjC() ||
           *PD->getKnownProtocolKind() == KnownProtocolKind::AnyObject);
    forwardDeclare(PD, [&]{ os << "@protocol " << PD->getName() << ";\n"; });
  }
  
  void forwardDeclare(const EnumDecl *ED) {
    assert(ED->isObjC() || ED->hasClangNode());
    
    forwardDeclare(ED, [&]{
      os << "enum " << ED->getName() << " : ";
      printer.print(ED->getRawType(), OTK_None);
      os << ";\n";
    });
  }

  void forwardDeclareMemberTypes(DeclRange members) {
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

      ReferencedTypeFinder::walk(VD->getType(),
                                 [this](ReferencedTypeFinder &finder,
                                        const TypeDecl *TD) {
        if (auto CD = dyn_cast<ClassDecl>(TD)) {
          if (!forwardDeclare(CD)) {
            bool didAddImport = addImport(CD);
            assert(didAddImport && "local class is not forward-declarable");
            (void)didAddImport;
          }
        } else if (auto PD = dyn_cast<ProtocolDecl>(TD))
          forwardDeclare(PD);
        else if (auto ED = dyn_cast<EnumDecl>(TD))
          forwardDeclare(ED);
        else if (addImport(TD))
          return;
        else if (auto TAD = dyn_cast<TypeAliasDecl>(TD))
          finder.visit(TAD->getUnderlyingType());
        else if (isa<AbstractTypeParamDecl>(TD))
          llvm_unreachable("should not see type params here");
        else
          assert(false && "unknown local type decl");
      });
    }

    declsToWrite.insert(declsToWrite.end()-1, nestedTypes.rbegin(),
                        nestedTypes.rend());

    // Separate forward declarations from the class itself.
    os << '\n';
  }

  bool writeClass(const ClassDecl *CD) {
    if (addImport(CD))
      return true;

    if (seenTypes[CD].first == EmissionState::Defined)
      return true;

    bool allRequirementsSatisfied = true;

    const ClassDecl *superclass = nullptr;
    if (Type superTy = CD->getSuperclass()) {
      superclass = superTy->getClassOrBoundGenericClass();
      allRequirementsSatisfied &= require(superclass);
    }
    for (auto proto : CD->getProtocols())
      if (printer.shouldInclude(proto))
        allRequirementsSatisfied &= require(proto);

    if (!allRequirementsSatisfied)
      return false;

    seenTypes[CD] = { EmissionState::Defined, true };
    forwardDeclareMemberTypes(CD->getMembers());
    printer.print(CD);
    return true;
  }

  bool writeProtocol(const ProtocolDecl *PD) {
    if (addImport(PD))
      return true;

    auto knownProtocol = PD->getKnownProtocolKind();
    if (knownProtocol && *knownProtocol == KnownProtocolKind::AnyObject)
      return true;

    if (seenTypes[PD].first == EmissionState::Defined)
      return true;

    bool allRequirementsSatisfied = true;

    for (auto proto : PD->getProtocols()) {
      assert(proto->isObjC());
      allRequirementsSatisfied &= require(proto);
    }

    if (!allRequirementsSatisfied)
      return false;

    seenTypes[PD] = { EmissionState::Defined, true };
    forwardDeclareMemberTypes(PD->getMembers());
    printer.print(PD);
    return true;
  }

  bool writeExtension(const ExtensionDecl *ED) {
    bool allRequirementsSatisfied = true;

    const ClassDecl *CD = ED->getExtendedType()->getClassOrBoundGenericClass();
    allRequirementsSatisfied &= require(CD);
    for (auto proto : ED->getProtocols())
      if (printer.shouldInclude(proto))
        allRequirementsSatisfied &= require(proto);

    if (!allRequirementsSatisfied)
      return false;

    forwardDeclareMemberTypes(ED->getMembers());
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
    return true;
  }

  void writePrologue(raw_ostream &out) {
    out << "// Generated by " << version::getSwiftFullVersion() << "\n"
           "#pragma clang diagnostic push\n"
           "\n"
           "#if defined(__has_include) && "
             "__has_include(<swift/objc-prologue.h>)\n"
           "# include <swift/objc-prologue.h>\n"
           "#endif\n"
           "\n"
           "#pragma clang diagnostic ignored \"-Wauto-import\"\n"
           "#include <objc/NSObject.h>\n"
           "#include <stdint.h>\n"
           "#include <stddef.h>\n"
           "#include <stdbool.h>\n"
           "\n"
           "#if defined(__has_include) && __has_include(<uchar.h>)\n"
           "# include <uchar.h>\n"
           "#elif !defined(__cplusplus) || __cplusplus < 201103L\n"
           "typedef uint_least16_t char16_t;\n"
           "typedef uint_least32_t char32_t;\n"
           "#endif\n"
           "\n"
           "typedef struct _NSZone NSZone;\n"
           "\n"
           "#if !defined(SWIFT_PASTE)\n"
           "# define SWIFT_PASTE_HELPER(x, y) x##y\n"
           "# define SWIFT_PASTE(x, y) SWIFT_PASTE_HELPER(x, y)\n"
           "#endif"
           "\n"
           "#if !defined(SWIFT_METATYPE)\n"
           "# define SWIFT_METATYPE(X) Class\n"
           "#endif\n"
           "\n"
           "#if defined(__has_attribute) && "
             "__has_attribute(objc_runtime_name)\n"
           "# define SWIFT_RUNTIME_NAME(X) "
             "__attribute__((objc_runtime_name(X)))\n"
           "#else\n"
           "# define SWIFT_RUNTIME_NAME(X)\n"
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
           "# if defined(__has_attribute) && "
             "__has_attribute(objc_subclassing_restricted) \n"
           "#  define SWIFT_CLASS(SWIFT_NAME) SWIFT_RUNTIME_NAME(SWIFT_NAME) "
             "__attribute__((objc_subclassing_restricted)) "
             "SWIFT_CLASS_EXTRA\n"
           "# else\n"
           "#  define SWIFT_CLASS(SWIFT_NAME) SWIFT_RUNTIME_NAME(SWIFT_NAME) "
             "SWIFT_CLASS_EXTRA\n"
           "# endif\n"
           "#endif\n"
           "\n"
           "#if !defined(SWIFT_PROTOCOL)\n"
           "# define SWIFT_PROTOCOL(SWIFT_NAME) SWIFT_RUNTIME_NAME(SWIFT_NAME) "
             "SWIFT_PROTOCOL_EXTRA\n"
           "#endif\n"
           "\n"
           "#if !defined(SWIFT_EXTENSION)\n"
           "# define SWIFT_EXTENSION(M) SWIFT_PASTE(M##_Swift_, __LINE__)\n"
           "#endif\n"
           "\n"
           "#if !defined(OBJC_DESIGNATED_INITIALIZER)\n"
           "# if defined(__has_attribute) && "
             "__has_attribute(objc_designated_initializer)\n"
           "#  define OBJC_DESIGNATED_INITIALIZER "
             "__attribute__((objc_designated_initializer))\n"
           "# else\n"
           "#  define OBJC_DESIGNATED_INITIALIZER\n"
           "# endif\n"
           "#endif\n"
           "#if !defined(SWIFT_ENUM)\n"
           "# define SWIFT_ENUM(_type, _name) "
             "enum _name : _type _name; "
             "enum SWIFT_ENUM_EXTRA _name : _type\n"
           "#endif\n"
           "#if __has_feature(nullability)\n"
           "#  define SWIFT_NULLABILITY(X) X\n"
           "#else\n"
           "# if !defined(__nonnull)\n"
           "#  define __nonnull\n"
           "# endif\n"
           "# if !defined(__nullable)\n"
           "#  define __nullable\n"
           "# endif\n"
           "#  define SWIFT_NULLABILITY(X)\n"
           "#endif\n";
  }

  bool isUnderlyingModule(Module *import) {
    if (bridgingHeader.empty())
      return import != &M && import->Name == M.Name;

    auto importer =
      static_cast<ClangImporter *>(import->Ctx.getClangModuleLoader());
    return import == importer->getImportedHeaderModule();
  }

  void writeImports(raw_ostream &out) {
    out << "#if defined(__has_feature) && __has_feature(modules)\n";

    // Track printed names to handle overlay modules.
    llvm::SmallPtrSet<Identifier, 8> seenImports;
    bool includeUnderlying = false;
    for (auto import : imports) {
      if (auto *swiftModule = import.dyn_cast<Module *>()) {
        auto Name = swiftModule->Name;
        if (isUnderlyingModule(swiftModule)) {
          includeUnderlying = true;
          continue;
        }
        if (seenImports.insert(Name).second)
          out << "@import " << Name.str() << ";\n";
      } else {
        const auto *clangModule = import.get<const clang::Module *>();
        out << "@import ";
        // FIXME: This should be an API on clang::Module.
        SmallVector<StringRef, 4> submoduleNames;
        do {
          submoduleNames.push_back(clangModule->Name);
          clangModule = clangModule->Parent;
        } while (clangModule);
        interleave(submoduleNames.rbegin(), submoduleNames.rend(),
                   [&out](StringRef next) { out << next; },
                   [&out] { out << "."; });
        out << ";\n";
      }
    }

    out << "#endif\n\n";

    if (includeUnderlying) {
      if (bridgingHeader.empty())
        out << "#import <" << M.Name.str() << '/' << M.Name.str() << ".h>\n\n";
      else
        out << "#import \"" << bridgingHeader << "\"\n\n";
    }

#ifndef SWIFT_DISABLE_OBJC_GENERICS
    // Once we've printed out the imports, deal with Foundations that
    // don't provide NS_ARRAY/NS_DICTIONARY/NS_SET.
    out << "#ifndef NS_ARRAY\n"
           "#  pragma clang diagnostic push\n"
           "#  pragma clang diagnostic ignored \"-Wvariadic-macros\"\n"
           "#  define NS_ARRAY(...) NSArray *\n"
           "#  pragma clang diagnostic pop\n"
           "#endif\n"
           "#ifndef NS_DICTIONARY\n"
           "#  pragma clang diagnostic push\n"
           "#  pragma clang diagnostic ignored \"-Wvariadic-macros\"\n"
           "#  define NS_DICTIONARY(...) NSDictionary *\n"
           "#  pragma clang diagnostic pop\n"
           "#endif\n"
           "#ifndef NS_SET\n"
           "#  pragma clang diagnostic push\n"
           "#  pragma clang diagnostic ignored \"-Wvariadic-macros\"\n"
           "#  define NS_SET(...) NSSet *\n"
           "#  pragma clang diagnostic pop\n"
           "#endif\n";
#endif
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
          return VD->getName().str();

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
      auto lhsProtos = cast<ExtensionDecl>(*lhs)->getProtocols();
      auto rhsProtos = cast<ExtensionDecl>(*rhs)->getProtocols();
      if (lhsProtos.size() != rhsProtos.size())
        return lhsProtos.size() < rhsProtos.size() ? Descending : Ascending;

      // If that fails, arbitrarily pick the extension whose protocols are
      // alphabetically first.
      auto mismatch =
        std::mismatch(lhsProtos.begin(), lhsProtos.end(), rhsProtos.begin(),
                      [getSortName] (const ProtocolDecl *nextLHSProto,
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

    writePrologue(out);
    writeImports(out);
    out <<
        "#pragma clang diagnostic ignored \"-Wproperty-attribute-mismatch\"\n"
        "#pragma clang diagnostic ignored \"-Wduplicate-method-arg\"\n"
      << os.str()
      << "#pragma clang diagnostic pop\n";
    return false;
  }
};
}

bool swift::printAsObjC(llvm::raw_ostream &os, Module *M,
                        StringRef bridgingHeader,
                        Accessibility minRequiredAccess) {
  return ModuleWriter(*M, bridgingHeader, minRequiredAccess).writeToStream(os);
}
