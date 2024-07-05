//===-- ExecutorOptions.h - ABI structures for executor options -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Swift ABI describing executor options.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_EXECUTOR_OPTIONS_H
#define SWIFT_ABI_EXECUTOR_OPTIONS_H

#include "swift/ABI/Executor.h"
#include "swift/ABI/HeapObject.h"
#include "swift/ABI/Metadata.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/ABI/TaskLocal.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Runtime/Config.h"
#include "llvm/Support/Casting.h"

namespace swift {

/// The abstract base class for all options that may be used
/// to configure how we should "check" an executor..
class ExecutorCheckOptionRecord {
public:
  ExecutorCheckOptionRecordFlags Flags;
  ExecutorCheckOptionRecord* _Nullable Parent;

  ExecutorCheckOptionRecord(
      ExecutorCheckOptionRecordKind kind, ExecutorCheckOptionRecordFlags flags,
      ExecutorCheckOptionRecord *_Nullable parent = nullptr)
      : Parent(parent) {
    flags.setKind(kind);
    Flags = flags;
  }

  ExecutorCheckOptionRecord(const ExecutorCheckOptionRecord &) = delete;
  ExecutorCheckOptionRecord &operator=(const ExecutorCheckOptionRecord &) = delete;

  ExecutorCheckOptionRecordKind getKind() const {
    return Flags.getKind();
  }

  bool isTaskLocalAllocated() const {
    return Flags.isTaskLocalAllocated();
  }

  ExecutorCheckOptionRecord* _Nullable getParent() const {
    return Parent;
  }

  /// Appropriately destroys the record using a task-local deallocation or free(),
  /// depending on how it was created.
  void destroy();
};


/******************************************************************************/
/************************ EXECUTOR CHECK OPTIONS ******************************/
/******************************************************************************/

/// Represents the #function, #file, #line that are often used when performing
/// executor checks, and may be used in providing better diagnostic logs.
class SourceLocationExecutorCheckOptionRecord : public ExecutorCheckOptionRecord {
public:
  const char * _Nonnull FunctionName;
  const char * _Nonnull File;
  const uintptr_t Line;
  const uintptr_t Column;

  SourceLocationExecutorCheckOptionRecord(
      ExecutorCheckOptionRecordFlags flags, const char *_Nonnull functionName,
      const char *_Nonnull file, uintptr_t line, uintptr_t column,
      ExecutorCheckOptionRecord *_Nullable Parent)
      : ExecutorCheckOptionRecord(
            ExecutorCheckOptionRecordKind::SourceLocation, flags, Parent),
        FunctionName(functionName), File(file), Line(line), Column(column) {}

  static bool classof(const ExecutorCheckOptionRecord * _Nonnull record) {
    return record->getKind() == ExecutorCheckOptionRecordKind::SourceLocation;
  }
};

SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
ExecutorCheckOptionRecord* _Nonnull swift_task_makeExecutorCheckOption_sourceLocation(
        const char* _Nonnull functionName, const char* _Nonnull file,
        uintptr_t line, uintptr_t column, ExecutorCheckOptionRecord* _Nullable parent);

SWIFT_EXPORT_FROM(swift_Concurrency) SWIFT_CC(swift)
void swift_task_destroyExecutorCheckOptions(ExecutorCheckOptionRecord * _Nonnull options);


} // end namespace swift

#endif // SWIFT_ABI_EXECUTOR_OPTIONS_H
