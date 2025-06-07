//===--- CancellableResult.h ------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_CANCELLABLE_RESULT_H
#define SWIFT_IDE_CANCELLABLE_RESULT_H

#include <string>

namespace swift {

namespace ide {

enum class CancellableResultKind { Success, Failure, Cancelled };

/// A result type that can carry one be in one of the following states:
///  - Success and carry a value of \c ResultType
///  - Failure and carry an error description
///  - Cancelled in case the operation that produced the result was cancelled
///
/// Essentially this emulates an enum with associated values as follows
/// \code
/// enum CancellableResult<ResultType> {
///   case success(ResultType)
///   case failure(String)
///   case cancelled
/// }
/// \endcode
///
/// The implementation is inspired by llvm::optional_detail::OptionalStorage
template <typename ResultType>
class CancellableResult {
  CancellableResultKind Kind;
  union {
    /// If \c Kind == Success, carries the result.
    ResultType Result;
    /// If \c Kind == Error, carries the error description.
    std::string Error;
    /// If \c Kind == Cancelled, this union is not initialized.
    char Empty;
  };

  CancellableResult(ResultType Result)
      : Kind(CancellableResultKind::Success), Result(Result) {}

  CancellableResult(std::string Error)
      : Kind(CancellableResultKind::Failure), Error(Error) {}

  explicit CancellableResult()
      : Kind(CancellableResultKind::Cancelled), Empty() {}

public:
  CancellableResult(const CancellableResult &Other) : Kind(Other.Kind), Empty() {
    switch (Kind) {
    case CancellableResultKind::Success:
      ::new ((void *)std::addressof(Result)) ResultType(Other.Result);
      break;
    case CancellableResultKind::Failure:
      ::new ((void *)std::addressof(Error)) std::string(Other.Error);
      break;
    case CancellableResultKind::Cancelled:
      break;
    }
  }

  CancellableResult(CancellableResult &&Other) : Kind(Other.Kind), Empty() {
    switch (Kind) {
    case CancellableResultKind::Success:
      ::new ((void *)std::addressof(Result))
          ResultType(std::move(Other.Result));
      break;
    case CancellableResultKind::Failure:
      ::new ((void *)std::addressof(Error)) std::string(std::move(Other.Error));
      break;
    case CancellableResultKind::Cancelled:
      break;
    }
  }

  ~CancellableResult() {
    using std::string;
    switch (Kind) {
    case CancellableResultKind::Success:
      Result.~ResultType();
      break;
    case CancellableResultKind::Failure:
      Error.~string();
      break;
    case CancellableResultKind::Cancelled:
      break;
    }
  }

  /// Construct a \c CancellableResult that carries a successful result.
  static CancellableResult success(ResultType Result) {
    return std::move(CancellableResult(ResultType(Result)));
  }

  /// Construct a \c CancellableResult that carries the error message of a
  /// failure.
  static CancellableResult failure(std::string Error) {
    return std::move(CancellableResult(Error));
  }

  /// Construct a \c CancellableResult representing that the producing operation
  /// was cancelled.
  static CancellableResult cancelled() {
    return std::move(CancellableResult());
  }

  /// Return the result kind this \c CancellableResult represents: success,
  /// failure or cancelled.
  CancellableResultKind getKind() const { return Kind; }

  /// Assuming that the result represents success, return the underlying result
  /// value.
  const ResultType &getResult() const {
    assert(getKind() == CancellableResultKind::Success);
    return Result;
  }

  /// Assuming that the result represents success, retrieve members of the
  /// underlying result value.
  const ResultType *operator->() { return &getResult(); }

  /// Assuming that the result represents success, return the underlying result
  /// value.
  const ResultType &operator*() { return getResult(); }

  /// Assuming that the result represents a failure, return the error message.
  std::string getError() const {
    assert(getKind() == CancellableResultKind::Failure);
    return Error;
  }

  /// If the result represents success, invoke \p Transform to asynchronously
  /// transform the wrapped result type and produce a new result type that is
  /// provided by the callback function passed to \p Transform. Afterwards call
  /// \p Handle with either the transformed value or the failure or cancelled
  /// result.
  /// The \c async part of the map means that the transform might happen
  /// asyncronously. This function does not introduce asynchronicity by itself.
  /// \p Transform might also invoke the callback synchronously.
  template <typename NewResultType>
  void
  mapAsync(llvm::function_ref<
               void(const ResultType &,
                    llvm::function_ref<void(CancellableResult<NewResultType>)>)>
               Transform,
           llvm::function_ref<void(CancellableResult<NewResultType>)> Handle) {
    switch (getKind()) {
    case CancellableResultKind::Success:
      Transform(getResult(), [&](CancellableResult<NewResultType> NewResult) {
        Handle(NewResult);
      });
      break;
    case CancellableResultKind::Failure:
      Handle(CancellableResult<NewResultType>::failure(getError()));
      break;
    case CancellableResultKind::Cancelled:
      Handle(CancellableResult<NewResultType>::cancelled());
      break;
    }
  }

  /// If the result represents success, invoke \p Transform to create a success
  /// result containing new backing data.
  ///
  /// If the result represents error or cancelled, propagate that kind without
  /// modification.
  template <typename NewResultType>
  CancellableResult<NewResultType>
  map(llvm::function_ref<NewResultType(const ResultType &)> Transform) {
    switch (getKind()) {
    case CancellableResultKind::Success:
      return CancellableResult<NewResultType>::success(Transform(getResult()));
    case CancellableResultKind::Failure:
      return CancellableResult<NewResultType>::failure(getError());
    case CancellableResultKind::Cancelled:
      return CancellableResult<NewResultType>::cancelled();
    }
  }
};

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_CANCELLABLE_RESULT_H
