#include <strings.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct ClassMetadata ClassMetadata;
typedef struct HeapObject HeapObject;

#if !__has_attribute(swiftcall)
#error "The runtime must be built with a compiler that supports swiftcall."
#endif

typedef struct ClassMetadata {
  ClassMetadata *superclassMetadata;
  void __attribute__((swiftcall)) (* destroy)(__attribute__((swift_context)) HeapObject *object);
} ClassMetadata;

typedef struct HeapObject {
  ClassMetadata *metadata;
  size_t refcount;
} HeapObject;

void *swift_slowAlloc(size_t bytes, size_t alignMask) {
  void *r = NULL;
  posix_memalign(&r, (alignMask == (size_t)-1) ? 16 : (alignMask + 1), bytes);
  bzero(r, bytes);
  return r;
}

void swift_slowDealloc(void *ptr, size_t bytes, size_t alignMask) {
  free(ptr);
}

void *swift_allocObject(ClassMetadata *metadata, size_t requiredSize, size_t requiredAlignmentMask) {
  HeapObject *object = swift_slowAlloc(requiredSize, requiredAlignmentMask);
  object->metadata = metadata;
  object->refcount = 1;
  return object;
}

void swift_deallocClassInstance(HeapObject *object, size_t allocatedSize, size_t allocatedAlignMask) {
  free(object);
}

HeapObject *swift_initStackObject(ClassMetadata *metadata, HeapObject *object) {
  object->metadata = metadata;
  object->refcount = -1;
  return object;
}

bool swift_isUniquelyReferenced_nonNull_native(HeapObject *object) {
  return object->refcount == 1;
}

void swift_release(HeapObject *object) {
  if (object == NULL) return;
  if (object->refcount == -1) return;

  object->refcount -= 1;
  if (object->refcount == 0) {
    object->metadata->destroy(object);
  }
}

HeapObject *swift_retain(HeapObject *object) {
  if (object->refcount == -1) return object;

  object->refcount += 1;
  return object;
}

void swift_beginAccess(void *pointer, void *buffer, uintptr_t flags, void *pc) { }
void swift_endAccess(void *buffer) { }

void swift_once(uintptr_t *predicate, void (*fn)(void *), void *context) {
  if (!*predicate) {
    *predicate = 1;
    fn(context);
  }
}
