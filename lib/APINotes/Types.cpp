//===--- Types.cpp - API Notes Data Types ----------------------*- C++ -*-===//
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
//
// This file defines data types used in the representation of API notes data.
//
//===----------------------------------------------------------------------===//
#include "swift/APINotes/Types.h"
#include "llvm/Support/raw_ostream.h"

void swift::api_notes::ObjCMethodInfo::dump(llvm::raw_ostream &os) {
    os << DesignatedInit << " " << FactoryAsInit << " " << Unavailable << " "
       << NullabilityAudited << " " << NumAdjustedNullable << " "
       << NullabilityPayload << " " << UnavailableMsg << "\n";
}

void swift::api_notes::ObjCContextInfo::dump(llvm::raw_ostream &os) {
  os << HasDefaultNullability << " " << DefaultNullability << " "
     << HasDesignatedInits << "\n";
}

void swift::api_notes::ObjCMethodInfo::mergePropInfoIntoSetter(
      const ObjCPropertyInfo &pInfo) {
  // Set the type of the first argument of the the setter or check that the
  // value we have is consistent with the property.
  // TODO: Can we provide proper error handling here?
  if (auto pNullability = pInfo.getNullability()) {
    if (*pNullability == NullableKind::Absent)
      return;
    if (!NullabilityAudited) {
      addParamTypeInfo(0, *pNullability);
      assert(NumAdjustedNullable == 2);
    } else {
      assert(getParamTypeInfo(0) == *pNullability);
    }
  }
}

void swift::api_notes::ObjCMethodInfo::mergePropInfoIntoGetter(
      const ObjCPropertyInfo &pInfo) {
  // Set the return type of the getter or check that the value we have is
  // consistent with the property.
  // TODO: Can we provide proper error handling here?
  if (auto pNullability = pInfo.getNullability()) {
    if (*pNullability == NullableKind::Absent)
      return;
    if (!NullabilityAudited) {
      addReturnTypeInfo(*pNullability);
      assert(NumAdjustedNullable == 1);
    } else {
      assert(getReturnTypeInfo() == *pNullability);
    }
  }
}
