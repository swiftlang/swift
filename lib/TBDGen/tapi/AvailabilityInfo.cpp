//===- lib/Core/AvailabilityInfo.cpp - Availability Info --------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "AvailabilityInfo.h"
#include "LLVM.h"

using namespace llvm;

TAPI_NAMESPACE_INTERNAL_BEGIN

void AvailabilityInfo::print(raw_ostream &os) const {
  os << "i:" << _introduced << " o:" << _obsoleted
     << " u:" << static_cast<int>((bool)_unavailable);
}

TAPI_NAMESPACE_INTERNAL_END
