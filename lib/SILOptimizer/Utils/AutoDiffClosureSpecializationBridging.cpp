//===--- AutoDiffClosureSpecializationBridging.cpp ------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "autodiff-closure-specialization-bridging"

#include "swift/SILOptimizer/AutoDiffClosureSpecializationBridging.h"
#include "swift/SILOptimizer/Differentiation/ADContext.h"
#include "swift/SILOptimizer/Differentiation/LinearMapInfo.h"

using namespace swift;

static SILType getBranchingTraceEnumLoweredTypeImpl(EnumDecl *ed,
                                                    SILFunction &vjp) {
  return autodiff::getLoweredTypeImpl(
      ed->getDeclaredInterfaceType()->getCanonicalType(), &vjp,
      vjp.getModule().Types);
}

BridgedType getBranchingTraceEnumLoweredType(BridgedEnumDecl ed,
                                              BridgedFunction vjp) {
  return getBranchingTraceEnumLoweredTypeImpl(ed.unbridged(),
                                              *vjp.getFunction());
}

BridgedType getBranchingTraceEnumLoweredType(BridgedDeclObj ed,
                                             BridgedFunction vjp) {
  return getBranchingTraceEnumLoweredTypeImpl(ed.getAs<EnumDecl>(),
                                              *vjp.getFunction());
}

BridgedNullableGenericParamList
cloneGenericParameters(BridgedASTContext ctx, BridgedDeclContext dc,
                       BridgedCanGenericSignature sig) {
  return autodiff::cloneGenericParameters(ctx.unbridged(), dc.unbridged(),
                                          sig.unbridged());
}

BridgedSourceFile autodiffGetSourceFile(BridgedFunction f) {
  return {&autodiff::getSourceFile(f.getFunction())};
}

BridgedOwnedString getEnumDeclAsString(BridgedType bteType) {
  std::string str;
  llvm::raw_string_ostream out(str);
  bteType.unbridged().getEnumOrBoundGenericEnum()->print(out);
  return BridgedOwnedString(/*stringToCopy=*/StringRef(str));
}
