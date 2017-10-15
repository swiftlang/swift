//===--- SILGlobalVariable.cpp - Defines SILGlobalVariable structure ------===//
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

#include "swift/SIL/SILGlobalVariable.h"
#include "swift/SIL/SILLinkage.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

SILGlobalVariable *SILGlobalVariable::create(SILModule &M, SILLinkage linkage,
                                             IsSerialized_t isSerialized,
                                             StringRef name,
                                             SILType loweredType,
                                             Optional<SILLocation> loc,
                                             VarDecl *Decl) {
  // Get a StringMapEntry for the variable.  As a sop to error cases,
  // allow the name to have an empty string.
  llvm::StringMapEntry<SILGlobalVariable*> *entry = nullptr;
  if (!name.empty()) {
    entry = &*M.GlobalVariableMap.insert(std::make_pair(name, nullptr)).first;
    assert(!entry->getValue() && "global variable already exists");
    name = entry->getKey();
  }

  auto var = new (M) SILGlobalVariable(M, linkage, isSerialized, name,
                                       loweredType, loc, Decl);

  if (entry) entry->setValue(var);
  return var;
}


SILGlobalVariable::SILGlobalVariable(SILModule &Module, SILLinkage Linkage,
                                     IsSerialized_t isSerialized,
                                     StringRef Name, SILType LoweredType,
                                     Optional<SILLocation> Loc, VarDecl *Decl)
  : Module(Module),
    Name(Name),
    LoweredType(LoweredType),
    Location(Loc),
    Linkage(unsigned(Linkage)),
    VDecl(Decl) {
  setSerialized(isSerialized);
  IsDeclaration = isAvailableExternally(Linkage);
  setLet(Decl ? Decl->isLet() : false);
  Module.silGlobals.push_back(this);
}

SILGlobalVariable::~SILGlobalVariable() {
  getModule().GlobalVariableMap.erase(Name);
}

/// Get this global variable's fragile attribute.
IsSerialized_t SILGlobalVariable::isSerialized() const {
  return Serialized ? IsSerialized : IsNotSerialized;
}
void SILGlobalVariable::setSerialized(IsSerialized_t isSerialized) {
  assert(isSerialized != IsSerializable);
  Serialized = isSerialized ? 1 : 0;
}

/// Return the value that is written into the global variable.
SILInstruction *SILGlobalVariable::getStaticInitializerValue() {
  if (StaticInitializerBlock.empty())
    return nullptr;

  return &StaticInitializerBlock.back();
}

bool SILGlobalVariable::isValidStaticInitializerInst(const SILInstruction *I,
                                                     SILModule &M) {
  switch (I->getKind()) {
    case SILInstructionKind::BuiltinInst: {
      auto *bi = cast<BuiltinInst>(I);
      switch (M.getBuiltinInfo(bi->getName()).ID) {
        case BuiltinValueKind::PtrToInt:
          if (isa<LiteralInst>(bi->getArguments()[0]))
            return true;
          break;
        default:
          break;
      }
      return false;
    }
    case SILInstructionKind::StringLiteralInst:
      switch (cast<StringLiteralInst>(I)->getEncoding()) {
        case StringLiteralInst::Encoding::UTF8:
        case StringLiteralInst::Encoding::UTF16:
          return true;
        case StringLiteralInst::Encoding::ObjCSelector:
          // Objective-C selector string literals cannot be used in static
          // initializers.
          return false;
      }
      return false;
    case SILInstructionKind::StructInst:
    case SILInstructionKind::TupleInst:
    case SILInstructionKind::IntegerLiteralInst:
    case SILInstructionKind::FloatLiteralInst:
    case SILInstructionKind::ObjectInst:
      return true;
    default:
      return false;
  }
}

/// Return whether this variable corresponds to a Clang node.
bool SILGlobalVariable::hasClangNode() const {
  return (VDecl ? VDecl->hasClangNode() : false);
}

/// Return the Clang node associated with this variable if it has one.
ClangNode SILGlobalVariable::getClangNode() const {
  return (VDecl ? VDecl->getClangNode() : ClangNode());
}
const clang::Decl *SILGlobalVariable::getClangDecl() const {
  return (VDecl ? VDecl->getClangDecl() : nullptr);
}
