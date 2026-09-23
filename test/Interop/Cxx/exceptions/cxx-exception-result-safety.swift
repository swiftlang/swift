//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-swift-frontend -typecheck %s -cxx-interoperability-mode=default -strict-memory-safety -verify -verify-ignore-unrelated

import Cxx

func missingInitialization() throws {
  // The compiler-only helper requires initialization on success, even when
  // the closure itself never performs a pointer operation.
  let _: Int = try _withCxxExceptionResult(Int.self) { _, _, _ in } // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}} expected-note {{reference to unsafe global function '_withCxxExceptionResult'}}
}
