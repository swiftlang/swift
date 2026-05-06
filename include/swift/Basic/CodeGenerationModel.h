//===--- CodeGenerationModel.h - Code generation model ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides a description of the code generation model.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_BASIC_CODEGENERATIONMODEL_H
#define SWIFT_BASIC_CODEGENERATIONMODEL_H

#include <cstdint>

namespace swift {

/// Describes where the code associated with a declaration is generated,
/// whether into the resulting object file (hiding the implementation),
/// Swift module for clients (exposing the implementation), or both.
///
/// The code generation model can be affected by a number of factors. For a
/// specific declaration, `@export` attribute introduced in SE-0497
/// explicitly chooses between "interface" and "implementation", while the
/// inlinability attributes choose "inlinable".
///
/// Globally, Embedded Swift defaults to "inlinable" but can be set to
/// "implementation" with the experimental feature DeferredCodeGen.
/// Non-embedded Swift uses "interface" and doesn't currently allow
/// customization.
enum class CodeGenerationModel: uint8_t {
  /// Only the interface of the declaration is made available, and clients can
  /// call through that interface. This is the equivalent to
  /// `@export(interface)` on a specific declaration.
  Interface,
  /// Both the interface and the implementation are made available to clients,
  /// who can choose whether to inline the implementation or call the
  /// implementation. This is equivalent to @inlinable.
  Inlinable,
  /// The implementation of the declaration is made available for the client to
  /// inline when it is used. There is no implementation in the corresponding
  /// module. This is the equivalent of `@export(implementation)` on a
  /// specific declaration.
  Implementation,
};
  
}

#endif 
