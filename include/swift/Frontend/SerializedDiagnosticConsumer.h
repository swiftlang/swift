//===--- SerializedDiagnosticConsumer.h - Serialize Diagnostics -*- C++ -*-===//
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
//
//  This file defines the SerializedDiagnosticConsumer class, which
//  serializes diagnostics to Clang's serialized diagnostic bitcode format.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SERIALIZEDDIAGNOSTICCONSUMER_H
#define SWIFT_SERIALIZEDDIAGNOSTICCONSUMER_H

#include <memory>

namespace llvm {
  class raw_ostream;
}

namespace swift {

  class DiagnosticConsumer;

  namespace serialized_diagnostics {
    /// \brief Create a DiagnosticConsumer that serializes diagnostics to a
    ///        stream.
    ///
    /// \param OS the stream to emit the diagnostics.  The consumer takes
    ///        ownership of the stream.
    ///
    /// \returns A new diagnostic consumer that serializes diagnostics.
    DiagnosticConsumer *createConsumer(std::unique_ptr<llvm::raw_ostream> OS);
  }
}

#endif
