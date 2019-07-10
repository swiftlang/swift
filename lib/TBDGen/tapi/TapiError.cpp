//===- lib/Core/TapiError.cpp - Tapi Error ----------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements TAPI Error.
///
//===----------------------------------------------------------------------===//

#include "TapiError.h"

using namespace llvm;

TAPI_NAMESPACE_INTERNAL_BEGIN

char TapiError::ID = 0;

void TapiError::log(raw_ostream &os) const {
  switch (ec) {
  case TapiErrorCode::NoSuchArchitecture:
    os << "no such architecture\n";
    return;
  }
  llvm_unreachable("unhandled TapiErrorCode");
}

std::error_code TapiError::convertToErrorCode() const {
  llvm_unreachable("convertToErrorCode is not supported.");
}

TAPI_NAMESPACE_INTERNAL_END
