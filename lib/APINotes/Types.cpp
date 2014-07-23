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
