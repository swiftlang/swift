//===--- RemoteAST.h - Relating remote metadata to local ASTs ---*- C++ -*-===//
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
//  The RemoteAST library defines interfaces for exploring the
//  relationships between a remote process and a local AST which provides
//  a (static and incomplete) picture of the code running in that process.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REMOTEAST_REMOTEAST_H
#define SWIFT_REMOTEAST_REMOTEAST_H

#include "swift/Remote/Failure.h"
#include "swift/Remote/MemoryReader.h"
#include "swift/Basic/LLVM.h"
#include "swift/ABI/MetadataValues.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringRef.h"

#include <memory>

namespace swift {
class ASTContext;
class NominalTypeDecl;
class Type;

namespace remoteAST {

template <class T>
class Result {
  union Storage {
    remote::Failure Failure;
    T Success;

    Storage() {}
    Storage(const Storage &) = delete;
    Storage &operator=(const Storage &) = delete;
    ~Storage() {}
  };

  Storage S;
  bool IsSuccess;

  Result(bool isSuccess) : IsSuccess(isSuccess) {}

public:
  /*implicit*/ Result(const T &value) : IsSuccess(true) {
    new (&S.Success) T(value);
  }

  /*implicit*/ Result(T &&value) : IsSuccess(true) {
    new (&S.Success) T(std::move(value));
  }

  /*implicit*/ Result(remote::Failure &&failure) : IsSuccess(false) {
    new (&S.Failure) remote::Failure(std::move(failure));
  }

  Result(const Result &other) : IsSuccess(other.IsSuccess) {
    if (IsSuccess) {
      ::new (&S.Success) T(other.S.Success);
    } else {
      ::new (&S.Failure) remote::Failure(other.S.Failure);      
    }
  }

  Result(Result &&other) : IsSuccess(other.IsSuccess) {
    if (IsSuccess) {
      ::new (&S.Success) T(std::move(other.S.Success));
    } else {
      ::new (&S.Failure) remote::Failure(std::move(other.S.Failure));
    }
  }

  Result &operator=(const Result &other) {
    this->~Result();
    ::new (this) Result(other);
    return *this;
  }

  Result &operator=(Result &&other) {
    this->~Result();
    ::new (this) Result(std::move(other));
    return *this;
  }

  ~Result() {
    if (IsSuccess) {
      S.Success.~T();
    } else {
      S.Failure.~Failure();
    }
  }

  template <class... ArgTys>
  static Result emplaceSuccess(ArgTys &&...args) {
    Result result(true);
    ::new (&result.S.Success) T(std::forward<ArgTys>(args)...);
    return result;
  }

  template <class KindTy, class... ArgTys>
  static Result emplaceFailure(KindTy kind, ArgTys &&...args) {
    Result result(false);
    ::new (&result.S.Failure)
      remote::Failure(kind, std::forward<ArgTys>(args)...);
    return result;
  }

  explicit operator bool() const { return isSuccess(); }
  bool isSuccess() const { return IsSuccess; }
  bool isFailure() const { return !IsSuccess; }

  const remote::Failure &getFailure() const {
    assert(isFailure());
    return S.Failure;
  }

  const T &getValue() const & {
    assert(isSuccess());
    return S.Success;
  }

  T &&getValue() && {
    assert(isSuccess());
    return std::move(S.Success);
  }
};

/// A context for performing an operation relating the remote process with
/// the AST.  This may be discarded and recreated at any time without danger,
/// but reusing a context across multiple calls may allow some redundant work
/// to be avoided.
///
/// The operations on this type must not assert or crash for any of the
/// following reasons:
///   - the parameters do not satisfy preconditions
///   - the memory reader fails for any reason
///   - the remote address is invalid
///   - the remote address space is corrupt
///   - the remote address space contains valid-seeming constructs that
///     somehow violate local well-formedness rules
///   - the local AST does not contain information for the remote construct
///
/// The operations on this type *may* misbehave if the invariants of
/// the local AST have been violated.  This should not be a problem if the
/// local AST is the result of ordinary type-checking.
class RemoteASTContext {
  void *Impl;

public:
  explicit RemoteASTContext(ASTContext &ctx,
                            std::shared_ptr<remote::MemoryReader> reader);

  RemoteASTContext(const RemoteASTContext &) = delete;
  RemoteASTContext &operator=(const RemoteASTContext &) = delete;

  ~RemoteASTContext();

  /// Given an address which is supposedly of type metadata, try to
  /// resolve it to a specific type in the local AST.
  ///
  /// \param skipArtificial If true, the address may be an artificial type
  ///   wrapper that should be ignored.  For example, it could be a
  ///   dynamic subclass created by (e.g.) CoreData or KVO; if so, and this
  ///   flag is set, this method will implicitly ignore the subclass
  ///   and instead attempt to resolve a type for the first non-artificial
  ///   superclass.
  Result<Type>
  getTypeForRemoteTypeMetadata(remote::RemoteAddress address,
                               bool skipArtificial = false);

  /// Given an address which is supposedly of type metadata, try to
  /// resolve it to a specific MetadataKind value for its backing type.
  Result<MetadataKind>
  getKindForRemoteTypeMetadata(remote::RemoteAddress address);

  /// Given an address which is supposedly of a nominal type descriptor,
  /// try to resolve it to a specific nominal type declaration in the
  /// local AST.
  Result<NominalTypeDecl *>
  getDeclForRemoteNominalTypeDescriptor(remote::RemoteAddress address);

  /// Given a type in the local AST, try to resolve the offset of its
  /// member with the given name.  This supports:
  ///
  ///   - stored properties of structs
  ///   - stored properties of classes
  ///   - elements of tuples
  ///
  /// Failure may indicate that an offset for the property could not be
  /// resolved, or it may simply indicate that the property is not laid
  /// out at a known offset.
  ///
  /// If the caller has the address of type metadata for the type, it may
  /// pass it in; this may allow the implementation to compute an offset in
  /// situations where it otherwise cannot.
  Result<uint64_t> getOffsetOfMember(Type type,
                                     remote::RemoteAddress optMetadataAddress,
                                     StringRef memberName);

  /// Given a heap object, resolve its heap metadata.
  Result<remote::RemoteAddress>
  getHeapMetadataForObject(remote::RemoteAddress address);

  /// Given an existential and its static type, resolve its dynamic
  /// type and address. A single step of unwrapping is performed, i.e. if the
  /// value stored inside the existential is itself an existential, the
  /// caller can decide whether to iterate itself.
  Result<std::pair<Type, remote::RemoteAddress>>
  getDynamicTypeAndAddressForExistential(remote::RemoteAddress address,
                                         Type staticType);
};

Type getTypeForMangling(ASTContext &ctx,
                        StringRef mangling);

} // end namespace remoteAST
} // end namespace swift

#endif // SWIFT_REMOTEAST_REMOTEAST_H
