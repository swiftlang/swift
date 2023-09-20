#include <strings.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

size_t _swiftEmptyArrayStorage[] = { /*isa*/0, /*refcount*/0, /*count*/0, /*flags*/1 };

typedef struct HeapObject {
  void *metadata;
  size_t refcount;
} HeapObject;

void *swift_allocObject(void *metadata, size_t requiredSize, size_t requiredAlignmentMask) {
  void *r = NULL;
  posix_memalign(&r, requiredAlignmentMask + 1, requiredSize);
  bzero(r, requiredSize);
  HeapObject *object = r;
  object->metadata = metadata;
  object->refcount = 1;
  return object;
}

void swift_deallocClassInstance(HeapObject *object, size_t allocatedSize, size_t allocatedAlignMask) {
  // don't deallocate ¯\_(ツ)_/¯
}

HeapObject *swift_initStackObject(void *metadata, HeapObject *object) {
  object->metadata = metadata;
  object->refcount = 1;
  return object;
}

bool swift_isUniquelyReferenced_nonNull_native(HeapObject *object) {
  return object->refcount == 1;
}

void swift_release(HeapObject *object) {
  // don't release ¯\_(ツ)_/¯
}

HeapObject *swift_retain(HeapObject *object) {
  object->refcount += 1;
  return object;
}

void swift_setDeallocating(HeapObject *object) {
  // don't deallocate ¯\_(ツ)_/¯
}

void swift_beginAccess(void *pointer, void *buffer, uintptr_t flags, void *pc) { }
void swift_endAccess(void *buffer) { }
