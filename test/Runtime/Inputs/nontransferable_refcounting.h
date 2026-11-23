#include <stddef.h>
#include <stdint.h>

typedef void *(*AllocObjectFn)(const void *metadata, size_t requiredSize,
                               size_t requiredAlignmentMask);
typedef void *(*RetainFn)(void *object);
typedef void *(*RetainNFn)(void *object, uint32_t n);
typedef void (*ReleaseFn)(void *object);
typedef void (*ReleaseNFn)(void *object, uint32_t n);

extern AllocObjectFn _swift_allocObject;
extern RetainFn _swift_retain;
extern RetainNFn _swift_retain_n;
extern ReleaseFn _swift_release;
extern ReleaseNFn _swift_release_n;

struct RefCountingHooks {
  AllocObjectFn originalAllocObject;
  RetainFn originalRetain;
  RetainNFn originalRetainN;
  ReleaseFn originalRelease;
  ReleaseNFn originalReleaseN;

  int watchingNextAllocation;
  void *watchedObject;
  size_t watchedAtomicOperations;
};

// File-scope statics in an imported header don't get definitions.
static inline struct RefCountingHooks *getRefCountingHooks(void) {
  static struct RefCountingHooks hooks;
  return &hooks;
}

static inline void *watchingAllocObject(const void *metadata,
                                        size_t requiredSize,
                                        size_t requiredAlignmentMask) {
  struct RefCountingHooks *hooks = getRefCountingHooks();
  void *object =
      hooks->originalAllocObject(metadata, requiredSize, requiredAlignmentMask);
  if (hooks->watchingNextAllocation) {
    hooks->watchingNextAllocation = 0;
    hooks->watchedObject = object;
  }
  return object;
}

static inline void noteAtomicOperation(void *object) {
  struct RefCountingHooks *hooks = getRefCountingHooks();
  if (object == hooks->watchedObject)
    ++hooks->watchedAtomicOperations;
}

static inline void *watchingRetain(void *object) {
  noteAtomicOperation(object);
  return getRefCountingHooks()->originalRetain(object);
}

static inline void *watchingRetainN(void *object, uint32_t n) {
  noteAtomicOperation(object);
  return getRefCountingHooks()->originalRetainN(object, n);
}

static inline void watchingRelease(void *object) {
  noteAtomicOperation(object);
  getRefCountingHooks()->originalRelease(object);
}

static inline void watchingReleaseN(void *object, uint32_t n) {
  noteAtomicOperation(object);
  getRefCountingHooks()->originalReleaseN(object, n);
}

/// Atomic retains and releases only go through the hooks once allocation is
/// hooked, because that is what turns on the runtime's swizzling support.
static inline void installRefCountingHooks(void) {
  struct RefCountingHooks *hooks = getRefCountingHooks();
  hooks->originalAllocObject = _swift_allocObject;
  hooks->originalRetain = _swift_retain;
  hooks->originalRetainN = _swift_retain_n;
  hooks->originalRelease = _swift_release;
  hooks->originalReleaseN = _swift_release_n;
  _swift_allocObject = watchingAllocObject;
  _swift_retain = watchingRetain;
  _swift_retain_n = watchingRetainN;
  _swift_release = watchingRelease;
  _swift_release_n = watchingReleaseN;
}

static inline void watchNextAllocation(void) {
  struct RefCountingHooks *hooks = getRefCountingHooks();
  hooks->watchingNextAllocation = 1;
  hooks->watchedObject = NULL;
  hooks->watchedAtomicOperations = 0;
}

/// Returns the number of atomic retains and releases of the watched object.
static inline size_t stopWatching(void) {
  struct RefCountingHooks *hooks = getRefCountingHooks();
  hooks->watchingNextAllocation = 0;
  hooks->watchedObject = NULL;
  return hooks->watchedAtomicOperations;
}
