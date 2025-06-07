//===--- ASTDumper.h - Swift AST Dumper flags -------------------*- C++ -*-===//
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
//
// This file defines types that are used to control the level of detail printed
// by the AST dumper.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_AST_DUMPER_H
#define SWIFT_AST_AST_DUMPER_H

namespace swift {

/// Describes the nature of requests that should be kicked off, if any, to
/// compute members and top-level decls when dumping an AST.
enum class ASTDumpMemberLoading {
  /// Dump cached members if available; if they are not, do not kick off any
  /// parsing or type-checking requests.
  None,

  /// Dump parsed members, kicking off a parsing request if necessary to compute
  /// them, but not performing additional type-checking.
  Parsed,

  /// Dump all fully-type checked members, kicking off any requests necessary to
  /// compute them.
  TypeChecked,
};

} // namespace swift

#endif // SWIFT_AST_AST_DUMPER_H