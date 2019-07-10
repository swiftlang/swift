//===- tapi/Core/TapiError.h - TAPI Error -----------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Define TAPI specific error codes.
///
//===----------------------------------------------------------------------===//

#ifndef TAPI_CORE_TAPIERROR_H
#define TAPI_CORE_TAPIERROR_H

#include "LLVM.h"
#include "Defines.h"
#include "llvm/Support/Error.h"

TAPI_NAMESPACE_INTERNAL_BEGIN

enum class TapiErrorCode {
  NoSuchArchitecture,
};

class TapiError : public llvm::ErrorInfo<TapiError> {
public:
  static char ID;
  TapiErrorCode ec;

  TapiError(TapiErrorCode ec) : ec(ec) {}

  void log(raw_ostream &os) const override;
  std::error_code convertToErrorCode() const override;
};

TAPI_NAMESPACE_INTERNAL_END

#endif // TAPI_CORE_TAPIERROR_H
