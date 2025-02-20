//===--- ConcurrencyLayout.cpp - Concurrency Type Layout Checks -----------===//
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

#include "Concurrency/ExecutorImpl.h"
#include "runtime/GenericCacheEntry.h"
#include "swift/ABI/TargetLayout.h"
#include "swift/ABI/Task.h"
#include "gtest/gtest.h"

#include <memory>

namespace {
// Ensure that Job's layout is compatible with what Dispatch expects.
// Note: MinimalDispatchObjectHeader just has the fields we care about, it is
// not complete and should not be used for anything other than these asserts.
struct MinimalDispatchObjectHeader {
  const void *VTable;
  int Opaque0;
  int Opaque1;
  void *Linkage;
};
} // namespace

namespace swift {
// NOTE: this is not possible to mark as `constexpr` as `reinterpret_cast` is
// not permitted in `constexpr` statements. Additionally, even if we were able
// to get away from the `reinterpret_cast`, we cannot de-reference the `nullptr`
// in a `constexpr` statement. Finally, because we are checking the offsets of
// members in non-standard layouts, we must delay this check until runtime as
// the static computation is not well defined by the language standard.
template <typename Type, typename PMFType>
size_t offset_of(PMFType Type::*member) {
  return reinterpret_cast<uintptr_t>(
      std::addressof(static_cast<Type *>(nullptr)->*member));
}

#define offset_of(T, d) swift::offset_of(&T::d)
} // namespace swift

TEST(ConcurrencyLayout, JobLayout) {
  ASSERT_EQ(offset_of(swift::Job, metadata), offset_of(SwiftJob, metadata));
#if !SWIFT_CONCURRENCY_EMBEDDED
  ASSERT_EQ(offset_of(swift::Job, refCounts), offset_of(SwiftJob, refCounts));
#else
  ASSERT_EQ(offset_of(swift::Job, embeddedRefcount),
            offset_of(SwiftJob, refCounts));
#endif
  ASSERT_EQ(offset_of(swift::Job, SchedulerPrivate),
            offset_of(SwiftJob, schedulerPrivate));
  ASSERT_EQ(offset_of(swift::Job, SchedulerPrivate) +
                (sizeof(*swift::Job::SchedulerPrivate) * 0),
            offset_of(SwiftJob, schedulerPrivate) +
                (sizeof(*SwiftJob::schedulerPrivate) * 0));
  ASSERT_EQ(offset_of(swift::Job, SchedulerPrivate) +
                (sizeof(*swift::Job::SchedulerPrivate) * 1),
            offset_of(SwiftJob, schedulerPrivate) +
                (sizeof(*SwiftJob::schedulerPrivate) * 1));
  ASSERT_EQ(offset_of(swift::Job, Flags), offset_of(SwiftJob, flags));

  // Job Metadata field must match location of Dispatch VTable field.
  ASSERT_EQ(offset_of(swift::Job, metadata),
            offset_of(MinimalDispatchObjectHeader, VTable));

  // Dispatch Linkage field must match Job
  // SchedulerPrivate[DispatchLinkageIndex].
  ASSERT_EQ(offset_of(swift::Job, SchedulerPrivate) +
                (sizeof(*swift::Job::SchedulerPrivate) *
                 swift::Job::DispatchLinkageIndex),
            offset_of(MinimalDispatchObjectHeader, Linkage));
}

TEST(MetadataLayout, CacheEntryLayout) {
// error: 'Value' is a private member of
// 'swift::VariadicMetadataCacheEntryBase<swift::GenericCacheEntry>'
#if 0
  ASSERT_EQ(
      offset_of(swift::GenericCacheEntry, Value),
      offset_of(swift::GenericMetadataCacheEntry<swift::InProcess::StoredPointer>,
                Value));
#endif
}
