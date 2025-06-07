//===--- TLSKeys.h - Reserved TLS keys ------------------------ -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_THREADING_TLSKEYS_H
#define SWIFT_THREADING_TLSKEYS_H

namespace swift {

enum class tls_key {
  runtime,
  stdlib,
  compatibility50,
  concurrency_task,
  concurrency_executor_tracking_info,
  concurrency_fallback,
  observation_transaction
};

} // namespace swift

#endif // SWIFT_THREADING_TLSKEYS_H
