#include "COMIdentity.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32) && defined(__i386__)
#define COM_CALL __stdcall
#else
#define COM_CALL
#endif

struct Interface {
  const void *VTable;
  struct COMIdentityObject *Owner;
};
struct COMIdentityObject {
  uintptr_t Padding;
  struct Interface Source, Identity;
  void *SourcePointer;
  void *Object;
  const void *Metadata;
  void (*Destroy)(void *);
  int SupportsIdentity;
};
static uint32_t References, Queries, Destructions;

struct IID {
  uint32_t Data1;
  uint16_t Data2, Data3;
  uint8_t Data4[8];
};
static const struct IID IID_IUnknown = {
    0, 0, 0, {0xc0, 0, 0, 0, 0, 0, 0, 0x46}};
static const struct IID IID_ISource = {
    0x10000000, 0, 0, {0, 0, 0, 0, 0, 0, 0, 6}};
static const struct IID IID_ISwiftObject = {
    0x8e369447,
    0x5188,
    0x5ada,
    {0xb9, 0xec, 0x8f, 0xcb, 0x73, 0x2d, 0x22, 0x6b}};

static uint32_t COM_CALL AddRef(struct Interface *self) {
  assert(References);
  return ++References;
}
static uint32_t COM_CALL Release(struct Interface *self) {
  assert(References);
  uint32_t remaining = --References;
  if (!remaining) {
    struct COMIdentityObject *owner = self->Owner;
    owner->Destroy(owner->Object);
    ++Destructions;
    free(owner);
  }
  return remaining;
}
static int32_t COM_CALL QueryInterface(struct Interface *self, const void *iid,
                                       void **result) {
  ++Queries;
  *result = NULL;
  if (!memcmp(iid, &IID_IUnknown, sizeof(struct IID)) ||
      !memcmp(iid, &IID_ISource, sizeof(struct IID)))
    *result = &self->Owner->Source;
  else if (self->Owner->SupportsIdentity &&
           !memcmp(iid, &IID_ISwiftObject, sizeof(struct IID)))
    *result = &self->Owner->Identity;
  else
    return (int32_t)0x80004002u;
  AddRef(*result);
  return 0;
}
static void *COM_CALL get_Object(struct Interface *self) {
  assert(self == &self->Owner->Identity);
  return self->Owner->Object;
}
static const void *COM_CALL get_Metadata(struct Interface *self) {
  assert(self == &self->Owner->Identity);
  return self->Owner->Metadata;
}
static const struct {
  int32_t(COM_CALL *QueryInterface)(struct Interface *, const void *, void **);
  uint32_t(COM_CALL *AddRef)(struct Interface *);
  uint32_t(COM_CALL *Release)(struct Interface *);
  void *(COM_CALL *get_Object)(struct Interface *);
  const void *(COM_CALL *get_Metadata)(struct Interface *);
} VTable = {QueryInterface, AddRef, Release, get_Object, get_Metadata};

struct COMIdentityObject *COMIdentityObject_Create(void *object,
                                                   const void *metadata,
                                                   void (*destroy)(void *),
                                                   int supportsIdentity) {
  assert(!References);
  struct COMIdentityObject *owner = calloc(1, sizeof(*owner));
  assert(owner);
  owner->Source = (struct Interface){&VTable, owner};
  owner->Identity = (struct Interface){&VTable, owner};
  owner->SourcePointer = &owner->Source;
  owner->Object = object;
  owner->Metadata = metadata;
  owner->Destroy = destroy;
  owner->SupportsIdentity = supportsIdentity;
  References = 1;
  Queries = Destructions = 0;
  return owner;
}
const void *COMIdentityObject_GetStorage(struct COMIdentityObject *object) {
  return &object->SourcePointer;
}
uint32_t COMIdentityObject_Release(struct COMIdentityObject *object) {
  return Release(&object->Source);
}
uint32_t COMIdentityObject_GetReferences(void) { return References; }
uint32_t COMIdentityObject_GetQueries(void) { return Queries; }
uint32_t COMIdentityObject_GetDestructions(void) { return Destructions; }
