//===--- TypeCheckUnasfe.h - Strict Safety Diagnostics ----------*- C++ -*-===//
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

#ifndef SWIFT_SEMA_TYPE_CHECK_UNSAFE_H
#define SWIFT_SEMA_TYPE_CHECK_UNSAFE_H

#include "swift/AST/UnsafeUse.h"

namespace llvm {
template <typename Fn> class function_ref;
}

namespace swift {

class Witness;

/// Diagnose the given unsafe use right now.
void diagnoseUnsafeUse(const UnsafeUse &use);

/// Enumerate all of the unsafe uses that occur within this declaration
///
/// The given `fn` will be called with each unsafe use. If it returns `true`
/// for any use, this function will return `true` immediately. Otherwise,
/// it will return `false` once all unsafe uses have been emitted.
bool enumerateUnsafeUses(ConcreteDeclRef declRef,
                         SourceLoc loc,
                         bool isCall,
                         bool skipTypeCheck,
                         llvm::function_ref<bool(UnsafeUse)> fn);

/// Enumerate all of the unsafe uses that occur within this array of protocol
/// conformances.
///
/// The given `fn` will be called with each unsafe use. If it returns `true`
/// for any use, this function will return `true` immediately. Otherwise,
/// it will return `false` once all unsafe uses have been emitted.
bool enumerateUnsafeUses(ArrayRef<ProtocolConformanceRef> conformances,
                         SourceLoc loc,
                         llvm::function_ref<bool(UnsafeUse)> fn);

/// Enumerate all of the unsafe uses that occur within this substitution map.
///
/// The given `fn` will be called with each unsafe use. If it returns `true`
/// for any use, this function will return `true` immediately. Otherwise,
/// it will return `false` once all unsafe uses have been emitted.
bool enumerateUnsafeUses(SubstitutionMap subs,
                         SourceLoc loc,
                         llvm::function_ref<bool(UnsafeUse)> fn);

/// Determine whether a reference to this declaration is considered unsafe,
/// either explicitly (@unsafe) or because it references an unsafe type.
bool isUnsafe(ConcreteDeclRef declRef);

/// Whether the given requirement should be considered unsafe for the given
/// conformance.
bool isUnsafeInConformance(const ValueDecl *requirement,
                           const Witness &witness,
                           NormalProtocolConformance *conformance);

/// If the given type involves an unsafe type, diagnose it by calling the
/// diagnose function with the most specific unsafe type that can be provided.
void diagnoseUnsafeType(ASTContext &ctx, SourceLoc loc, Type type,
                        llvm::function_ref<void(Type)> diagnose);

/// Check for unsafe storage within this nominal type declaration.
void checkUnsafeStorage(NominalTypeDecl *nominal);

}

#endif // SWIFT_SEMA_TYPE_CHECK_UNSAFE_H
