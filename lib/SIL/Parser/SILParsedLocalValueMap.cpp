//===--- ParseSIL.cpp - SIL File Parsing logic ----------------------------===//
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

#include "SILParsedLocalValueMap.h"
#include "swift/Basic/LLVM.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILUndef.h"

using namespace swift;
using namespace swift::syntax;

namespace swift {

template <class... DiagArgs, class... Args>
InFlightDiagnostic SILParsedLocalValueMap::diagnose(SourceLoc loc,
                                                    Diag<DiagArgs...> diagID,
                                                    Args &&... args) {
  return module.getASTContext().Diags.diagnose(
      loc, Diagnostic(diagID, std::forward<Args>(args)...));
}

SILValue SILParsedLocalValueMap::getLocalValue(UnresolvedValueName name,
                                               SILType type, SILLocation loc,
                                               SILBuilder &builder) {
  if (name.isUndef())
    return SILUndef::get(type, builder.getFunction());

  // Check to see if a value with this name already exists.
  SILValue &val = localValues[name.Name];

  if (val) {
    // If this value has already been defined, make sure it has the type we're
    // expecting.
    SILType valType = val->getType();

    if (valType != type) {
      diagnose(name.NameLoc, diag::sil_value_use_type_mismatch, name.Name,
               valType.getASTType(), type.getASTType());
      hadError();
    }

    return SILValue(val);
  }

  // Otherwise, this is a forward reference. Create a dummy global to represent
  // it.
  forwardReferences[name.Name] = name.NameLoc;

  val = new (module) GlobalAddrInst(
      SILDebugLocation(loc, builder.getFunction().getDebugScope()), type);
  return val;
}

void SILParsedLocalValueMap::setLocalValue(SILValue val, StringRef name,
                                           SourceLoc loc) {
  SILValue &oldValue = localValues[name];

  // If this value was already defined then it must be the definition of a
  // forward referenced value.
  if (oldValue) {
    // If no forward reference exists for it, then it's a redefinition so,
    // emit an error.
    if (!forwardReferences.erase(name)) {
      diagnose(loc, diag::sil_value_redefinition, name);
      hadError();
    }

    // If the forward reference was of the wrong type, diagnose this now.
    if (oldValue->getType() != val->getType()) {
      diagnose(loc, diag::sil_value_def_type_mismatch, name,
               oldValue->getType().getASTType(), val->getType().getASTType());
      hadError();
    } else {
      // Forward references only live here if they have a single result.
      oldValue->replaceAllUsesWith(val);
    }
  }
  // Update the old value to be this value.
  oldValue = val;
}

llvm::ArrayRef<std::pair<StringRef, SourceLoc>>
SILParsedLocalValueMap::getForwardRefs() {
  return {&*forwardReferences.begin(), &*forwardReferences.end()};
}

} // end namespace swift
