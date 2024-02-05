//===--- VariableNameUtils.cpp --------------------------------------------===//
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

#include "swift/SILOptimizer/Utils/VariableNameUtils.h"

using namespace swift;

SILValue
VariableNameInferrer::findDebugInfoProvidingValue(SILValue searchValue) {
  if (!searchValue)
    return SILValue();

  while (true) {
    assert(searchValue);
    if (auto *allocInst = dyn_cast<AllocationInst>(searchValue)) {
      // If the instruction itself doesn't carry any variable info, see
      // whether it's copied from another place that does.
      auto allocInstHasInfo = [](AllocationInst *allocInst) {
        if (allocInst->getDecl())
          return true;
        auto debugVar = DebugVarCarryingInst(allocInst);
        return debugVar && debugVar.maybeGetName().has_value();
      };

      if (!allocInstHasInfo(allocInst)) {
        if (auto copy = allocInst->getSingleUserOfType<CopyAddrInst>()) {
          if (copy->getDest() == allocInst && !copy->isTakeOfSrc() &&
              copy->isInitializationOfDest()) {
            searchValue = copy->getSrc();
            continue;
          }
        }

        // If we didn't find anything and did not have a decl, return SILValue()
        // so that we fail.
        return SILValue();
      }

      variableNamePath.push_back(allocInst);
      return allocInst;
    }

    if (auto *globalAddrInst = dyn_cast<GlobalAddrInst>(searchValue)) {
      variableNamePath.push_back(globalAddrInst);
      return globalAddrInst;
    }

    if (auto *oeInst = dyn_cast<OpenExistentialAddrInst>(searchValue)) {
      searchValue = oeInst->getOperand();
      continue;
    }

    if (auto *rei = dyn_cast<RefElementAddrInst>(searchValue)) {
      variableNamePath.push_back(rei);
      searchValue = rei->getOperand();
      continue;
    }

    if (auto *fArg = dyn_cast<SILFunctionArgument>(searchValue)) {
      variableNamePath.push_back({fArg});
      return fArg;
    }

    auto getNamePathComponentFromCallee =
        [&](FullApplySite call) -> std::optional<SILValue> {
      // Use the name of the property being accessed if we can get to it.
      if (isa<FunctionRefBaseInst>(call.getCallee()) ||
          isa<MethodInst>(call.getCallee())) {
        variableNamePath.push_back(call.getCallee()->getDefiningInstruction());
        // Try to name the base of the property if this is a method.
        if (call.getSubstCalleeType()->hasSelfParam()) {
          return call.getSelfArgument();
        }

        return SILValue();
      }
      return {};
    };

    // Read or modify accessor.
    if (auto bai = dyn_cast_or_null<BeginApplyInst>(
            searchValue->getDefiningInstruction())) {
      if (auto selfParam = getNamePathComponentFromCallee(bai)) {
        searchValue = *selfParam;
        continue;
      }
    }

    // Addressor accessor.
    if (auto ptrToAddr =
            dyn_cast<PointerToAddressInst>(stripAccessMarkers(searchValue))) {
      // The addressor can either produce the raw pointer itself or an
      // `UnsafePointer` stdlib type wrapping it.
      ApplyInst *addressorInvocation;
      if (auto structExtract =
              dyn_cast<StructExtractInst>(ptrToAddr->getOperand())) {
        addressorInvocation = dyn_cast<ApplyInst>(structExtract->getOperand());
      } else {
        addressorInvocation = dyn_cast<ApplyInst>(ptrToAddr->getOperand());
      }

      if (addressorInvocation) {
        if (auto selfParam =
                getNamePathComponentFromCallee(addressorInvocation)) {
          searchValue = *selfParam;
          continue;
        }
      }
    }

    // If we do not do an exact match, see if we can find a debug_var inst. If
    // we do, we always break since we have a root value.
    if (auto *use = getAnyDebugUse(searchValue)) {
      if (auto debugVar = DebugVarCarryingInst(use->getUser())) {
        assert(debugVar.getKind() == DebugVarCarryingInst::Kind::DebugValue);
        variableNamePath.push_back(use->getUser());

        // We return the value, not the debug_info.
        return searchValue;
      }
    }

    // Otherwise, try to see if we have a single value instruction we can look
    // through.
    if (isa<BeginBorrowInst>(searchValue) || isa<LoadInst>(searchValue) ||
        isa<LoadBorrowInst>(searchValue) || isa<BeginAccessInst>(searchValue) ||
        isa<MarkUnresolvedNonCopyableValueInst>(searchValue) ||
        isa<ProjectBoxInst>(searchValue) || isa<CopyValueInst>(searchValue)) {
      searchValue = cast<SingleValueInstruction>(searchValue)->getOperand(0);
      continue;
    }

    // Return SILValue() if we ever get to the bottom to signal we failed to
    // find anything.
    return SILValue();
  }
}

static StringRef getNameFromDecl(Decl *d) {
  if (d) {
    if (auto accessor = dyn_cast<AccessorDecl>(d)) {
      return accessor->getStorage()->getBaseName().userFacingName();
    }
    if (auto vd = dyn_cast<ValueDecl>(d)) {
      return vd->getBaseName().userFacingName();
    }
  }

  return "<unknown decl>";
}

void VariableNameInferrer::drainVariableNamePath() {
  // Walk backwards, constructing our string.
  while (true) {
    auto next = variableNamePath.pop_back_val();

    if (auto *inst = next.dyn_cast<SILInstruction *>()) {
      if (auto i = DebugVarCarryingInst(inst)) {
        resultingString += i.getName();
      } else if (auto i = VarDeclCarryingInst(inst)) {
        resultingString += i.getName();
      } else if (auto f = dyn_cast<FunctionRefBaseInst>(inst)) {
        if (auto dc = f->getInitiallyReferencedFunction()->getDeclContext()) {
          resultingString += getNameFromDecl(dc->getAsDecl());
        } else {
          resultingString += "<unknown decl>";
        }
      } else if (auto m = dyn_cast<MethodInst>(inst)) {
        resultingString += getNameFromDecl(m->getMember().getDecl());
      } else {
        resultingString += "<unknown decl>";
      }
    } else {
      auto value = next.get<SILValue>();
      if (auto *fArg = dyn_cast<SILFunctionArgument>(value))
        resultingString += fArg->getDecl()->getBaseName().userFacingName();
    }

    if (variableNamePath.empty())
      return;

    resultingString += '.';
  }
}
