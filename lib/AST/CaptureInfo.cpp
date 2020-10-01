//===--- CaptureInfo.cpp - Data Structure for Capture Lists ---------------===//
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

#include "swift/AST/CaptureInfo.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

CaptureInfo::CaptureInfo(ASTContext &ctx, ArrayRef<CapturedValue> captures,
                         DynamicSelfType *dynamicSelf,
                         OpaqueValueExpr *opaqueValue,
                         bool genericParamCaptures) {
  static_assert(IsTriviallyDestructible<CapturedValue>::value,
                "Capture info is alloc'd on the ASTContext and not destroyed");
  static_assert(IsTriviallyDestructible<CaptureInfo::CaptureInfoStorage>::value,
                "Capture info is alloc'd on the ASTContext and not destroyed");

  OptionSet<Flags> flags;
  if (genericParamCaptures)
    flags |= Flags::HasGenericParamCaptures;

  if (captures.empty() && !dynamicSelf && !opaqueValue) {
    *this = CaptureInfo::empty();
    StorageAndFlags.setInt(flags);
    return;
  }

  size_t storageToAlloc =
      CaptureInfoStorage::totalSizeToAlloc<CapturedValue>(captures.size());
  void *storageBuf = ctx.Allocate(storageToAlloc, alignof(CaptureInfoStorage));
  auto *storage = new (storageBuf) CaptureInfoStorage(captures.size(),
                                                      dynamicSelf,
                                                      opaqueValue);
  StorageAndFlags.setPointerAndInt(storage, flags);
  std::uninitialized_copy(captures.begin(), captures.end(),
                          storage->getTrailingObjects<CapturedValue>());
}

CaptureInfo CaptureInfo::empty() {
  static const CaptureInfoStorage empty{0, /*dynamicSelf*/nullptr,
                                        /*opaqueValue*/nullptr};
  CaptureInfo result;
  result.StorageAndFlags.setPointer(&empty);
  return result;
}

bool CaptureInfo::hasLocalCaptures() const {
  for (auto capture : getCaptures())
    if (capture.getDecl()->isLocalCapture())
      return true;
  return false;
}


void CaptureInfo::
getLocalCaptures(SmallVectorImpl<CapturedValue> &Result) const {
  if (!hasLocalCaptures()) return;

  Result.reserve(getCaptures().size());

  // Filter out global variables.
  for (auto capture : getCaptures()) {
    if (!capture.getDecl()->isLocalCapture())
      continue;

    Result.push_back(capture);
  }
}

LLVM_ATTRIBUTE_USED void CaptureInfo::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void CaptureInfo::print(raw_ostream &OS) const {
  OS << "captures=(";

  if (hasGenericParamCaptures())
    OS << "<generic> ";
  if (hasDynamicSelfCapture())
    OS << "<dynamic_self> ";
  if (hasOpaqueValueCapture())
    OS << "<opaque_value> ";

  interleave(getCaptures(),
             [&](const CapturedValue &capture) {
               OS << capture.getDecl()->getBaseName();

               if (capture.isDirect())
                 OS << "<direct>";
               if (capture.isNoEscape())
                 OS << "<noescape>";
             },
             [&] { OS << ", "; });
  OS << ')';
}

