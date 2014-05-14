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
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/Path.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

namespace {
class ObjCPrinter : private DeclVisitor<ObjCPrinter>,
                    private TypeVisitor<ObjCPrinter> {
  friend ASTVisitor;
  friend TypeVisitor;

  llvm::DenseMap<std::pair<Identifier, Identifier>, StringRef> specialNames;
  Identifier cConstPointerID;
  Identifier cMutablePointerID;
  Identifier objcMutablePointerID;
  Identifier unsafePointerID;
  Identifier unmanagedID;

  ASTContext &ctx;
  raw_ostream &os;

  SmallVector<const FunctionType *, 4> openFunctionTypes;

  bool protocolMembersOptional = false;

  friend ASTVisitor<ObjCPrinter>;
  friend TypeVisitor<ObjCPrinter>;

public:
  explicit ObjCPrinter(ASTContext &context, raw_ostream &out)
    : ctx(context), os(out) {}

  void print(const Decl *D) {
    visit(const_cast<Decl *>(D));
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
                 [](const ProtocolDecl *PD) -> bool {
      if (!PD->isObjC())
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
                 auto Name = PD->getName().str();
                 // Remap back to the original protocol name that may
                 // have been changed during import.
                 Name = llvm::StringSwitch<StringRef>(Name)
#define RENAMED_PROTOCOL(ObjCName, SwiftName) .Case(#SwiftName, #ObjCName)
#include "swift/ClangImporter/RenamedProtocols.def"
                  .Default(Name);
                 os << Name;
               },
               [this] { os << ", "; });
    os << ">";
  }

  /// Prints the members of a class, extension, or protocol.
  void printMembers(DeclRange members) {
    for (auto member : members) {
      auto VD = dyn_cast<ValueDecl>(member);
      if (!VD || !VD->isObjC())
        continue;
      if (auto FD = dyn_cast<FuncDecl>(VD))
        if (FD->isAccessor())
          continue;
      if (VD->getAttrs().isOptional() != protocolMembersOptional) {
        protocolMembersOptional = VD->getAttrs().isOptional();
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
    os << "@interface " << baseClass->getName() << " ()";
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
    assert(!protocolMembersOptional && "protocols start @required");
    printMembers(PD->getMembers());
    protocolMembersOptional = false;
    os << "@end\n";
  }

  StringRef printSingleMethodParam(StringRef selectorString,
                                   const Pattern *param) {
    StringRef firstPiece, restOfSelector;
    std::tie(firstPiece, restOfSelector) = selectorString.split(':');
    os << firstPiece << ":(";
    this->print(param->getType());
    os << ")";

    if (isa<AnyPattern>(param))
      os << "_";
    else
      os << cast<NamedPattern>(param)->getBoundName();

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
      os << "instancetype";
    } else {
      print(methodTy->getResult());
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
      if (ctor->isDesignatedInit()) {
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

  void visitVarDecl(VarDecl *VD) {
    assert(VD->getDeclContext()->isTypeContext() &&
           "cannot handle global variables right now");
    assert(!VD->isStatic() && "class properties cannot be @objc");

    printDocumentationComment(VD);

    // For now, never promise atomicity.
    os << "@property (nonatomic";

    if (!VD->isSettable(nullptr))
      os << ", readonly";

    // FIXME: Include "weak", "strong", "assign" here.
    // They aren't actually needed (they won't change runtime semantics), but
    // they provide documentation and improve the quality of warnings.

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
    print(VD->getType(), VD->getName().str());
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
  void visitPart(Type ty) {
    TypeVisitor::visit(ty);
  }

  /// If "name" is one of the standard library types used to map in Clang
  /// primitives and basic types, print out the appropriate spelling and
  /// return true.
  ///
  /// This handles typealiases and structs provided by the standard library
  /// for interfacing with C and Objective-C.
  bool printIfKnownTypeName(Identifier moduleName, Identifier name) {
    if (specialNames.empty()) {
#define MAP(SWIFT_NAME, CLANG_REPR) \
      specialNames[{ctx.StdlibModuleName, ctx.getIdentifier(#SWIFT_NAME)}] = \
        CLANG_REPR

      MAP(CBool, "bool");

      MAP(CChar, "char");
      MAP(CWideChar, "wchar_t");
      MAP(CChar16, "char16_t");
      MAP(CChar32, "char32_t");

      MAP(CSignedChar, "signed char");
      MAP(CShort, "short");
      MAP(CInt, "int");
      MAP(CLong, "long");
      MAP(CLongLong, "long long");

      MAP(CUnsignedChar, "unsigned char");
      MAP(CUnsignedShort, "unsigned short");
      MAP(CUnsignedInt, "unsigned int");
      MAP(CUnsignedLong, "unsigned long");
      MAP(CUnsignedLongLong, "unsigned long long");

      MAP(CFloat, "float");
      MAP(CDouble, "double");

      MAP(Int8, "int8_t");
      MAP(Int16, "int16_t");
      MAP(Int32, "int32_t");
      MAP(Int64, "int64_t");
      MAP(UInt8, "uint8_t");
      MAP(UInt16, "uint16_t");
      MAP(UInt32, "uint32_t");
      MAP(UInt64, "uint64_t");

      MAP(Float, "float");
      MAP(Double, "double");
      MAP(Float32, "float");
      MAP(Float64, "double");

      MAP(Int, "NSInteger");
      MAP(UInt, "NSUInteger");
      MAP(Bool, "BOOL");
      MAP(String, "NSString *");

      MAP(COpaquePointer, "void *");
      MAP(CMutableVoidPointer, "void *");
      MAP(CConstVoidPointer, "void const *");

      Identifier ID_ObjectiveC = ctx.getIdentifier(OBJC_MODULE_NAME);
      specialNames[{ID_ObjectiveC, ctx.getIdentifier("ObjCBool")}] = "BOOL";
      specialNames[{ID_ObjectiveC, ctx.getIdentifier("Selector")}] = "SEL";

      Identifier ID_Foundation = ctx.getIdentifier(FOUNDATION_MODULE_NAME);
      specialNames[{ID_Foundation, ctx.getIdentifier("NSZone")}] = "NSZone *";
    }

    auto iter = specialNames.find({moduleName, name});
    if (iter == specialNames.end())
      return false;
    os << iter->second;
    return true;
  }

  void visitType(TypeBase *Ty) {
    os << "/* ";
    Ty->print(os);
    os << " */";
  }

  void visitNameAliasType(NameAliasType *aliasTy) {
    const TypeAliasDecl *alias = aliasTy->getDecl();
    if (printIfKnownTypeName(alias->getModuleContext()->Name, alias->getName()))
      return;

    if (alias->hasClangNode() || alias->isObjC()) {
      os << alias->getName();
      return;
    }

    visitPart(alias->getUnderlyingType());
  }

  void visitStructType(StructType *ST) {
    const StructDecl *SD = ST->getStructOrBoundGenericStruct();
    if (printIfKnownTypeName(SD->getModuleContext()->Name, SD->getName()))
      return;

    // FIXME: Check if we can actually use the name or if we have to tag it with
    // "struct".
    os << SD->getName();
  }

  /// If \p BGT represents a generic struct used to import Clang types, print
  /// it out.
  bool printIfKnownGenericStruct(const BoundGenericStructType *BGT) {
    StructDecl *SD = BGT->getDecl();
    if (!SD->getModuleContext()->isStdlibModule())
      return false;

    if (SD == ctx.getArrayDecl()) {
      // FIXME: It'd be nice to put the element type here as well.
      os << "NSArray *";
      return true;
    }

    if (SD == ctx.getDictionaryDecl()) {
      // FIXME: IT'd be nice to put the element type here as well.
      os << "NSDictionary *";
      return true;
    }

    // Everything from here on is some kind of pointer type.
    if (unsafePointerID.empty()) {
      cConstPointerID = ctx.getIdentifier("CConstPointer");
      cMutablePointerID = ctx.getIdentifier("CMutablePointer");
      objcMutablePointerID = ctx.getIdentifier("ObjCMutablePointer");
      unsafePointerID = ctx.getIdentifier("UnsafePointer");
    }

    bool isConst;
    if (SD->getName() == cConstPointerID) {
      isConst = true;
    } else if (SD->getName() == cMutablePointerID ||
               SD->getName() == objcMutablePointerID ||
               SD->getName() == unsafePointerID) {
      isConst = false;
    } else {
      // Not a pointer.
      return false;
    }

    auto args = BGT->getGenericArgs();
    assert(args.size() == 1);
    visitPart(args.front());
    if (isConst)
      os << " const";
    os << " *";
    return true;
  }

  void visitBoundGenericStructType(BoundGenericStructType *BGT) {
    if (printIfKnownGenericStruct(BGT))
      return;
    visitBoundGenericType(BGT);
  }

  void visitBoundGenericType(BoundGenericType *BGT) {
    if (auto underlying = BGT->getAnyOptionalObjectType())
      visitPart(underlying);
    else
      visitType(BGT);
  }

  void visitEnumType(EnumType *ET) {
    const EnumDecl *ED = ET->getDecl();

    // FIXME: Check if we can actually use the name or if we have to tag it with
    // "enum".
    os << ED->getName();
  }

  void visitClassType(ClassType *CT) {
    const ClassDecl *CD = CT->getClassOrBoundGenericClass();
    assert(CD->isObjC());
    auto clangDecl = dyn_cast_or_null<clang::NamedDecl>(CD->getClangDecl());
    if (clangDecl) {
      if (isa<clang::ObjCInterfaceDecl>(clangDecl)) {
        os << clangDecl->getName() << " *";
      } else {
        os << clangDecl->getName();
      }
    } else {
      os << CD->getName() << " *";
    }
  }

  void visitProtocolType(ProtocolType *PT, bool isMetatype = false) {
    os << (isMetatype ? "Class" : "id");

    auto proto = PT->getDecl();
    assert(proto->isObjC());
    if (auto knownKind = proto->getKnownProtocolKind())
      if (*knownKind == KnownProtocolKind::AnyObject)
        return;

    printProtocols(proto);
  }

  void visitProtocolCompositionType(ProtocolCompositionType *PCT,
                                    bool isMetatype = false) {
    CanType canonicalComposition = PCT->getCanonicalType();
    if (auto singleProto = dyn_cast<ProtocolType>(canonicalComposition))
      return visitProtocolType(singleProto, isMetatype);
    PCT = cast<ProtocolCompositionType>(canonicalComposition);

    os << (isMetatype ? "Class" : "id");

    SmallVector<ProtocolDecl *, 4> protos;
    std::transform(PCT->getProtocols().begin(), PCT->getProtocols().end(),
                   std::back_inserter(protos),
                   [] (Type ty) -> ProtocolDecl * {
      return ty->castTo<ProtocolType>()->getDecl();
    });
    printProtocols(protos);
  }

  void visitExistentialMetatypeType(ExistentialMetatypeType *MT) {
    Type instanceTy = MT->getInstanceType();
    if (auto protoTy = instanceTy->getAs<ProtocolType>()) {
      visitProtocolType(protoTy, /*isMetatype=*/true);
    } else if (auto compTy = instanceTy->getAs<ProtocolCompositionType>()) {
      visitProtocolCompositionType(compTy, /*isMetatype=*/true);
    } else {
      visitType(MT);
    }
  }

  void visitMetatypeType(MetatypeType *MT) {
    Type instanceTy = MT->getInstanceType();
    if (auto classTy = instanceTy->getAs<ClassType>()) {
      const ClassDecl *CD = classTy->getDecl();
      if (CD->isObjC())
        os << "SWIFT_METATYPE(" << CD->getName() << ")";
      else
        os << "Class";
    } else {
      visitType(MT);
    }
  }

  void visitFunctionType(FunctionType *FT) {
    switch (FT->getRepresentation()) {
    case AnyFunctionType::Representation::Thin:
      assert(false && "can't handle thin functions yet");
    // Native Swift function types bridge to block types.
    case AnyFunctionType::Representation::Thick:
    case AnyFunctionType::Representation::Block:
      visitPart(FT->getResult());
      os << " (^";
      openFunctionTypes.push_back(FT);
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
    if (auto parenTy = dyn_cast<ParenType>(paramsTy.getPointer())) {
      print(parenTy->getSinglyDesugaredType());
    } else {
      auto tupleTy = cast<TupleType>(paramsTy.getPointer());
      if (tupleTy->getNumElements() == 0) {
        os << "void";
      } else {
        interleave(tupleTy->getElementTypes(),
                   [this](Type ty) { print(ty); },
                   [this] { os << ", "; });
      }
    }
    os << ")";
  }

  void visitTupleType(TupleType *TT) {
    assert(TT->getNumElements() == 0);
    os << "void";
  }

  void visitParenType(ParenType *PT) {
    visitPart(PT->getSinglyDesugaredType());
  }

  void visitSubstitutedType(SubstitutedType *ST) {
    visitPart(ST->getSinglyDesugaredType());
  }

  void visitSyntaxSugarType(SyntaxSugarType *SST) {
    visitPart(SST->getSinglyDesugaredType());
  }

  void visitDynamicSelfType(DynamicSelfType *DST) {
    os << "instancetype";
  }

  /// Print a full type, optionally declaring the given \p name.
  ///
  /// This will properly handle nested function types (see
  /// finishFunctionType()). If only a part of a type is being printed, use
  /// visitPart().
  void print(Type ty, StringRef name = "") {
    decltype(openFunctionTypes) savedFunctionTypes;
    savedFunctionTypes.swap(openFunctionTypes);

    visitPart(ty);
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

  std::function<void(ReferencedTypeFinder &, const TypeDecl *)> Callback;

  ReferencedTypeFinder(decltype(Callback) &&callback) : Callback(callback) {}

  void visitType(TypeBase *base) {
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

  template<typename Fn>
  static void walk(Type ty, const Fn &callback) {
    ReferencedTypeFinder(std::cref(callback)).visit(ty);
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
  llvm::SmallSetVector<Module *, 8> imports;

  std::string bodyBuffer;
  llvm::raw_string_ostream os{bodyBuffer};

  Module &M;
  ObjCPrinter printer;
public:
  ModuleWriter(Module &mod) : M(mod), printer(M.Ctx, os) {}

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
    imports.insert(otherModule);
    return true;
  }

  bool require(const TypeDecl *D) {
    if (addImport(D))
      return true;

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

  void forwardDeclare(const NominalTypeDecl *NTD, StringRef introducer) {
    if (addImport(NTD))
      return;
    auto &state = seenTypes[NTD];
    if (state.second)
      return;
    os << introducer << ' ' << NTD->getName() << ";\n";
    state.second = true;
  }

  void forwardDeclare(const ClassDecl *CD) {
    if (!CD->isObjC())
      return;
    forwardDeclare(CD, "@class");
  }

  void forwardDeclare(const ProtocolDecl *PD) {
    assert(PD->isObjC() ||
           *PD->getKnownProtocolKind() == KnownProtocolKind::AnyObject);
    forwardDeclare(PD, "@protocol");
  }

  void forwardDeclareMemberTypes(DeclRange members) {
    for (auto member : members) {
      auto VD = dyn_cast<ValueDecl>(member);
      if (!VD || !VD->isObjC())
        continue;
      ReferencedTypeFinder::walk(VD->getType(),
                                 [this](ReferencedTypeFinder &finder,
                                        const TypeDecl *TD) {
        if (addImport(TD))
          return;
        if (auto CD = dyn_cast<ClassDecl>(TD))
          forwardDeclare(CD);
        else if (auto PD = dyn_cast<ProtocolDecl>(TD))
          forwardDeclare(PD);
        else if (auto TAD = dyn_cast<TypeAliasDecl>(TD))
          finder.visit(TAD->getUnderlyingType());
        else if (isa<AbstractTypeParamDecl>(TD))
          llvm_unreachable("should not see type params here");
        else
          assert(false && "unknown local type decl");
      });
    }
    os << '\n';
  }

  bool writeClass(const ClassDecl *CD) {
    if (addImport(CD))
      return true;

    auto &state = seenTypes[CD];
    if (state.first == EmissionState::Defined)
      return true;

    bool allRequirementsSatisfied = true;

    const ClassDecl *superclass = nullptr;
    if (Type superTy = CD->getSuperclass()) {
      superclass = superTy->getClassOrBoundGenericClass();
      allRequirementsSatisfied &= require(superclass);
    }
    for (auto proto : CD->getProtocols())
      if (proto->isObjC())
        allRequirementsSatisfied &= require(proto);

    if (!allRequirementsSatisfied)
      return false;

    state = { EmissionState::Defined, true };
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

    auto &state = seenTypes[PD];
    if (state.first == EmissionState::Defined)
      return true;

    bool allRequirementsSatisfied = true;

    for (auto proto : PD->getProtocols()) {
      assert(proto->isObjC());
      allRequirementsSatisfied &= require(proto);
    }

    if (!allRequirementsSatisfied)
      return false;

    state = { EmissionState::Defined, true };
    forwardDeclareMemberTypes(PD->getMembers());
    printer.print(PD);
    return true;
  }

  bool writeExtension(const ExtensionDecl *ED) {
    bool allRequirementsSatisfied = true;

    const ClassDecl *CD = ED->getExtendedType()->getClassOrBoundGenericClass();
    allRequirementsSatisfied &= require(CD);
    for (auto proto : ED->getProtocols())
      if (proto->isObjC())
        allRequirementsSatisfied &= require(proto);

    if (!allRequirementsSatisfied)
      return false;

    forwardDeclareMemberTypes(ED->getMembers());
    printer.print(ED);
    return true;
  }

  void writePrologue(raw_ostream &out) {
    out << "// Generated by " << version::getSwiftFullVersion() << "\n"
           "\n"
           "#if defined(__has_include) && "
             "__has_include(<swift/objc-prologue.h>)\n"
           "# include <swift/objc-prologue.h>\n"
           "#endif\n"
           "\n"
           "#include <objc/NSObject.h>\n"
           "#include <stdint.h>\n"
           "#include <stddef.h>\n"
           "#include <stdbool.h>\n"
           "\n"
           "#if defined(__has_include) && __has_include(<uchar.h>)\n"
           "# include <uchar.h>\n"
           "#elif __cplusplus < 201103L\n"
           "typedef uint_least16_t char16_t;\n"
           "typedef uint_least32_t char32_t;\n"
           "#endif\n"
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
           "#if !defined(SWIFT_CLASS)\n"
           "# if defined(__has_attribute) && "
             "__has_attribute(objc_subclassing_restricted) \n"
           "#  if __has_attribute(objc_complete_definition)\n"
           "#   define SWIFT_CLASS(SWIFT_NAME) SWIFT_RUNTIME_NAME(SWIFT_NAME) "
             "__attribute__((objc_subclassing_restricted)) "
             "__attribute__((objc_complete_definition)) "
             "SWIFT_CLASS_EXTRA\n"
           "#  else\n"
           "#   define SWIFT_CLASS(SWIFT_NAME) SWIFT_RUNTIME_NAME(SWIFT_NAME) "
             "__attribute__((objc_subclassing_restricted)) "
             "SWIFT_CLASS_EXTRA\n"
           "#  endif\n"
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
           "#if !defined(OBJC_DESIGNATED_INITIALIZER)\n"
           "# if defined(__has_attribute) && "
             "__has_attribute(objc_designated_initializer)\n"
           "#  define OBJC_DESIGNATED_INITIALIZER "
             "__attribute__((objc_designated_initializer))\n"
           "# else\n"
           "#  define OBJC_DESIGNATED_INITIALIZER\n"
           "# endif\n"
           "#endif\n"
           "\n";
  }

  static bool isClangHeaderModule(Module *import) {
    auto importer =
      static_cast<ClangImporter *>(import->Ctx.getClangModuleLoader());
    return import == importer->getImportedHeaderModule();
  }

  void writeImports(raw_ostream &out) {
    out << "#if defined(__has_feature) && __has_feature(modules)\n";

    llvm::DenseSet<Identifier> seenImports;
    bool includeUmbrella = false;
    for (auto import : imports) {
      auto Name = import->Name;
      if (Name == M.Name) {
        includeUmbrella = true;
        continue;
      }
      if (isClangHeaderModule(import)) {
        // FIXME: Should we at least emit the header provided on the command
        // line with -import-objc-header?
        continue;
      }
      if (seenImports.insert(Name).second)
        out << "@import " << Name.str() << ";\n";
    }

    out << "#endif\n\n";

    if (includeUmbrella)
      out << "#import <" << M.Name.str() << '/' << M.Name.str() << ".h>\n\n";
  }

  bool writeToStream(raw_ostream &out) {
    SmallVector<Decl *, 64> decls;
    M.getTopLevelDecls(decls);

    auto newEnd = std::remove_if(decls.begin(), decls.end(),
                                 [] (const Decl *D) -> bool {
      if (auto VD = dyn_cast<ValueDecl>(D)) {
        // FIXME: Distinguish IBOutlet/IBAction from true interop.
        return !VD->isObjC();
      }

      if (auto ED = dyn_cast<ExtensionDecl>(D)) {
        auto baseClass = ED->getExtendedType()->getClassOrBoundGenericClass();
        return !baseClass || !baseClass->isObjC();
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
    out << os.str();

    return false;
  }
};
}

bool swift::printAsObjC(llvm::raw_ostream &os, Module *M) {
  return ModuleWriter(*M).writeToStream(os);
}
