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

#include "PrintAsObjC.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "clang/AST/Decl.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

static void writeImports(raw_ostream &os, Module *M) {
  SmallVector<Module::ImportedModule, 16> imports;
  M->getImportedModules(imports, /*includePrivate=*/true);
  for (auto import : imports) {
    // FIXME: Handle submodule imports.
    os << "@import " << import.second->Name << ";\n";
  }

  // Headers required by the printer.
  os << "\n@import ObjectiveC;\n"
        "#include <stdint.h>\n"
        "#include <stddef.h>\n"
        "#include <stdbool.h>\n"
        "#if defined(__has_include)\n"
        "# if __has_include(<uchar.h>)\n"
        "#  include <uchar.h>\n"
        "# endif\n"
        "#endif\n\n";
}

namespace {
class ObjCPrinter : public DeclVisitor<ObjCPrinter>,
                    public TypeVisitor<ObjCPrinter> {
  llvm::DenseMap<Identifier, StringRef> specialNames;
  ASTContext &ctx;
  raw_ostream &os;

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
  using TypeVisitor::visit;

  /// Prints a protocol adoption list: <code>&lt;NSCoding, NSCopying&gt;</code>
  ///
  /// This method filters out non-ObjC protocols, along with the special
  /// DynamicLookup protocol.
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
      return *knownProtocol != KnownProtocolKind::DynamicLookup;
    });

    if (protosToPrint.empty())
      return;

    os << " <";
    interleave(protosToPrint,
               [this](const ProtocolDecl *PD) { os << PD->getName(); },
               [this] { os << ", "; });
    os << ">";
  }

  /// Prints the members of a class, extension, or protocol.
  void printMembers(ArrayRef<Decl *> members) {
    for (auto member : members) {
      auto VD = dyn_cast<ValueDecl>(member);
      if (!VD || !VD->isObjC())
        continue;
      visit(VD);
    }
  }

  void visitClassDecl(ClassDecl *CD) {
    os << "@interface " << CD->getName();
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
    os << "@protocol " << PD->getName();
    printProtocols(PD->getProtocols());
    os << "\n";
    printMembers(PD->getMembers());
    os << "@end\n";
  }

  void visitFuncDecl(FuncDecl *FD) {
    assert(FD->getDeclContext()->isTypeContext() &&
           "cannot handle free functions right now");
    if (FD->isStatic())
      os << "+ (";
    else
      os << "- (";

    Type rawMethodTy = FD->getType()->castTo<AnyFunctionType>()->getResult();
    auto methodTy = rawMethodTy->castTo<FunctionType>();
    visit(methodTy->getResult());
    os << ")" << FD->getName();

    auto argPatterns = FD->getArgParamPatterns();
    assert(argPatterns.size() == 2 && "not an ObjC-compatible method");
    const TuplePattern *argParams = cast<TuplePattern>(argPatterns.back());
    assert(!argParams->hasVararg() && "can't handle variadic methods");

    auto bodyPatterns = FD->getBodyParamPatterns();
    assert(bodyPatterns.size() == 2 && "not an ObjC-compatible method");
    const TuplePattern *bodyParams = cast<TuplePattern>(bodyPatterns.back());

    bool isFirst = true;
    for_each(argParams->getFields(), bodyParams->getFields(),
             [this, &isFirst] (const TuplePatternElt &argParam,
                               const TuplePatternElt &bodyParam) {
      // FIXME: Handle default arguments.
      if (!isFirst) {
        auto argPattern = argParam.getPattern()->getSemanticsProvidingPattern();
        os << " " << cast<NamedPattern>(argPattern)->getBoundName();
      }

      auto bodyPattern = bodyParam.getPattern()->getSemanticsProvidingPattern();
      os << ":(";
      this->visit(bodyPattern->getType());
      os << ")";

      if (isa<AnyPattern>(bodyPattern)) {
        // FIXME: Do a better job synthesizing an initial argument name.
        os << "_";
      } else {
        os << cast<NamedPattern>(bodyPattern)->getBoundName();
      }

      isFirst = false;
    });

    os << ";\n";
  }

  /// If "name" is a
  ///
  /// This handles typealiases and structs provided by the standard library
  /// for interfacing with C and Objective-C.
  bool printIfKnownTypeName(Identifier name) {
    if (specialNames.empty()) {
#define MAP(SWIFT_NAME, CLANG_REPR) \
      specialNames[ctx.getIdentifier(#SWIFT_NAME)] = CLANG_REPR

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
      MAP(Bool, "BOOL");
      MAP(String, "NSString *");
      MAP(COpaquePointer, "void *");
    }

    auto iter = specialNames.find(name);
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

  void visitStructType(StructType *ST) {
    const StructDecl *SD = ST->getStructOrBoundGenericStruct();
    if (SD->getParentModule()->isStdlibModule())
      if (printIfKnownTypeName(SD->getName()))
        return;

    // FIXME: Check if we can actually use the name or if we have to tag it with
    // "struct".
    os << SD->getName();
  }

  void visitNameAliasType(NameAliasType *aliasTy) {
    const TypeAliasDecl *alias = aliasTy->getDecl();
    if (alias->getModuleContext()->isStdlibModule())
      if (printIfKnownTypeName(alias->getName()))
        return;

    if (alias->hasClangNode() || alias->isObjC()) {
      os << alias->getName();
      return;
    }

    visit(alias->getUnderlyingType());
  }

  void visitClassType(ClassType *CT) {
    const ClassDecl *CD = CT->getClassOrBoundGenericClass();
    os << CD->getName() << " *";
  }

  void visitProtocolType(ProtocolType *PT, bool isMetatype = false) {
    os << (isMetatype ? "Class" : "id");

    auto proto = PT->getDecl();
    if (auto knownKind = proto->getKnownProtocolKind())
      if (*knownKind == KnownProtocolKind::DynamicLookup)
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

  void visitMetatypeType(MetatypeType *MT) {
    Type instanceTy = MT->getInstanceType();
    if (auto protoTy = instanceTy->getAs<ProtocolType>())
      visitProtocolType(protoTy, /*isMetatype=*/true);
    else if (auto compositionTy = instanceTy->getAs<ProtocolCompositionType>())
      visitProtocolCompositionType(compositionTy, /*isMetatype=*/true);
    else
      llvm_unreachable("Arbitrary metatypes are not ObjC-compatible");
  }

  void visitTupleType(TupleType *TT) {
    assert(TT->getNumElements() == 0);
    os << "void";
  }

  void visitParenType(ParenType *PT) {
    visit(PT->getSinglyDesugaredType());
  }

  void visitSubstitutedType(SubstitutedType *ST) {
    visit(ST->getSinglyDesugaredType());
  }

  void visitSyntaxSugarType(SyntaxSugarType *SST) {
    visit(SST->getSinglyDesugaredType());
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
  raw_ostream &os;
  Module &M;
  ObjCPrinter printer;
public:
  ModuleWriter(raw_ostream &out, Module &mod)
    : os(out), M(mod), printer(M.Ctx, os) {}

  bool isLocal(const Decl *D) {
    return D->getModuleContext() == &M;
  }

  bool require(const TypeDecl *D) {
    if (!isLocal(D))
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

  void forwardDeclare(const ClassDecl *CD) {
    if (!isLocal(CD))
      return;
    auto &state = seenTypes[CD];
    if (state.second)
      return;
    os << "@class " << CD->getName() << ";\n";
    state.second = true;
  }

  void forwardDeclare(const ProtocolDecl *PD) {
    if (!isLocal(PD))
      return;
    auto &state = seenTypes[PD];
    if (state.second)
      return;
    os << "@protocol " << PD->getName() << ";\n";
    state.second = true;
  }

  bool writeClass(const ClassDecl *CD) {
    if (!isLocal(CD))
      return true;

    auto &state = seenTypes[CD];
    if (state.first == EmissionState::Defined)
      return true;

    size_t pendingSize = declsToWrite.size();

    const ClassDecl *superclass = nullptr;
    if (Type superTy = CD->getSuperclass()) {
      superclass = superTy->getClassOrBoundGenericClass();
      require(superclass);
    }
    for (auto proto : CD->getProtocols())
      if (proto->isObjC())
        require(proto);

    if (declsToWrite.size() != pendingSize)
      return false;

    printer.print(CD);
    state = { EmissionState::Defined, true };
    return true;
  }

  bool writeProtocol(const ProtocolDecl *PD) {
    if (!isLocal(PD))
      return true;

    auto knownProtocol = PD->getKnownProtocolKind();
    if (knownProtocol && *knownProtocol == KnownProtocolKind::DynamicLookup)
      return true;

    auto &state = seenTypes[PD];
    if (state.first == EmissionState::Defined)
      return true;

    size_t pendingSize = declsToWrite.size();

    for (auto proto : PD->getProtocols()) {
      assert(proto->isObjC());
      require(proto);
    }

    if (declsToWrite.size() != pendingSize)
      return false;

    printer.print(PD);
    state = { EmissionState::Defined, true };
    return true;
  }

  bool writeExtension(const ExtensionDecl *ED) {
    size_t pendingSize = declsToWrite.size();

    const ClassDecl *CD = ED->getExtendedType()->getClassOrBoundGenericClass();
    require(CD);
    for (auto proto : ED->getProtocols())
      if (proto->isObjC())
        require(proto);

    if (declsToWrite.size() != pendingSize)
      return false;

    printer.print(ED);
    return true;
  }

  bool writeDecls() {
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
      auto lhsMembers = cast<ExtensionDecl>(*lhs)->getMembers();
      auto rhsMembers = cast<ExtensionDecl>(*rhs)->getMembers();
      if (lhsMembers.size() != rhsMembers.size())
        return lhsMembers.size() < rhsMembers.size() ? Descending : Ascending;

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

    return false;
  }
};
}

static bool writeDecls(raw_ostream &os, Module *M) {
  return ModuleWriter(os, *M).writeDecls();
}

int swift::doPrintAsObjC(const CompilerInvocation &InitInvok) {
  CompilerInstance CI;
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);

  if (CI.setup(InitInvok))
    return 1;
  CI.performParse();

  if (CI.getASTContext().hadError())
    return 1;

  writeImports(llvm::outs(), CI.getMainModule());
  bool HadError = writeDecls(llvm::outs(), CI.getMainModule());

  return HadError;
}
