//===----------------------------------------------------------------------===//
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

#include "swift/Runtime/Config.h"

#if SWIFT_OBJC_INTEROP
#include "swift/Basic/Lazy.h"
#include "swift/Basic/LLVM.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Mutex.h"
#include "swift/Runtime/ObjCBridge.h"
#include <vector>
#import <Foundation/Foundation.h>
#import <CoreFoundation/CoreFoundation.h>

using namespace swift;

/// Class of sentinel objects used to represent the `nil` value of nested
/// optionals.
@interface _SwiftNull : NSObject {
@public
  unsigned depth;
}
@end

@implementation _SwiftNull : NSObject

- (NSString*)description {
  return [NSString stringWithFormat:@"<%@ %p depth = %u>", [self class],
                                                           (void*)self,
                                                           self->depth];
}

@end

namespace {

struct SwiftNullSentinelCache {
  std::vector<id> Cache;
  StaticReadWriteLock Lock;
};

static Lazy<SwiftNullSentinelCache> Sentinels;

static id getSentinelForDepth(unsigned depth) {
  // For unnested optionals, use NSNull.
  if (depth == 1)
    return id_const_cast(kCFNull);
  // Otherwise, make up our own sentinel.
  // See if we created one for this depth.
  auto &theSentinels = Sentinels.get();
  unsigned depthIndex = depth - 2;
  {
    StaticScopedReadLock lock(theSentinels.Lock);
    const auto &cache = theSentinels.Cache;
    if (depthIndex < cache.size()) {
      id cached = cache[depthIndex];
      if (cached)
        return cached;
    }
  }
  // Make one if we need to.
  {
    StaticScopedWriteLock lock(theSentinels.Lock);
    if (depthIndex >= theSentinels.Cache.size())
      theSentinels.Cache.resize(depthIndex + 1);
    auto &cached = theSentinels.Cache[depthIndex];
    // Make sure another writer didn't sneak in.
    if (!cached) {
      auto sentinel = [[_SwiftNull alloc] init];
      sentinel->depth = depth;
      cached = sentinel;
    }
    return cached;
  }
}

}

/// Return the sentinel object to use to represent `nil` for a given Optional
/// type.
SWIFT_RUNTIME_STDLIB_API SWIFT_CC(swift)
id _swift_Foundation_getOptionalNilSentinelObject(const Metadata *Wrapped) {
  // Figure out the depth of optionality we're working with.
  unsigned depth = 1;
  while (Wrapped->getKind() == MetadataKind::Optional) {
    ++depth;
    Wrapped = cast<EnumMetadata>(Wrapped)->getGenericArgs()[0];
  }

  return objc_retain(getSentinelForDepth(depth));
}
#endif

