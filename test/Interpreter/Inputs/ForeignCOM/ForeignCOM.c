#include "ForeignCOM.h"
#include <assert.h>
#include <stdlib.h>

#if defined(_WIN32) && defined(__i386__)
#define COM_CALL __stdcall
#else
#define COM_CALL
#endif

struct Interface {
  const void *VTable;
  struct ForeignCOMObject *Owner;
};

struct ForeignCOMObject {
  struct Interface Value;
  struct Interface Property;
  const void *ValueInterface;
  const void *PropertyInterface;
  uint32_t References;
  int32_t StoredValue;
};

static uint32_t ActiveReferences;
static uint32_t AddRefs, Releases, Destructions, MethodCalls;

static uint32_t COM_CALL AddRef(struct Interface *self) {
  assert(self->Owner->References);
  ++ActiveReferences;
  ++AddRefs;
  return ++self->Owner->References;
}

static uint32_t COM_CALL Release(struct Interface *self) {
  struct ForeignCOMObject *object = self->Owner;
  assert(object->References);
  --ActiveReferences;
  ++Releases;
  uint32_t remaining = --object->References;
  if (!remaining) {
    ++Destructions;
    free(object);
  }
  return remaining;
}

// Querying a different interface is outside this fixture's scope.
static int32_t COM_CALL QueryInterface(struct Interface *self, const void *iid,
                                       void **result) {
  abort();
}

static int32_t COM_CALL Value(struct Interface *self, int32_t offset) {
  assert(self == &self->Owner->Value);
  ++MethodCalls;
  return self->Owner->StoredValue + offset;
}

static int32_t COM_CALL Multiply(struct Interface *self, int32_t factor) {
  assert(self == &self->Owner->Value);
  ++MethodCalls;
  return self->Owner->StoredValue * factor;
}

static int32_t COM_CALL get_Value(struct Interface *self) {
  assert(self == &self->Owner->Property);
  ++MethodCalls;
  return self->Owner->StoredValue;
}

static void COM_CALL put_Value(struct Interface *self, int32_t value) {
  assert(self == &self->Owner->Property);
  ++MethodCalls;
  self->Owner->StoredValue = value;
}

static int32_t COM_CALL get_Item(struct Interface *self, int32_t index) {
  assert(self == &self->Owner->Property);
  ++MethodCalls;
  return self->Owner->StoredValue + index;
}

static void COM_CALL Reset(struct Interface *self) {
  assert(self == &self->Owner->Property);
  ++MethodCalls;
  self->Owner->StoredValue = 0;
}

struct IdentityVTable {
  int32_t(COM_CALL *QueryInterface)(struct Interface *, const void *, void **);
  uint32_t(COM_CALL *AddRef)(struct Interface *);
  uint32_t(COM_CALL *Release)(struct Interface *);
};

static const struct {
  struct IdentityVTable Identity;
  int32_t(COM_CALL *Value)(struct Interface *, int32_t);
  int32_t(COM_CALL *Multiply)(struct Interface *, int32_t);
} ValueVTable = {{QueryInterface, AddRef, Release}, Value, Multiply};

static const struct {
  struct IdentityVTable Identity;
  int32_t(COM_CALL *get_Value)(struct Interface *);
  void(COM_CALL *put_Value)(struct Interface *, int32_t);
  int32_t(COM_CALL *get_Item)(struct Interface *, int32_t);
  void(COM_CALL *Reset)(struct Interface *);
} PropertyVTable = {
    {QueryInterface, AddRef, Release}, get_Value, put_Value, get_Item, Reset};

struct ForeignCOMObject *ForeignCOMObject_Create(int32_t value) {
  // Each test starts after all references from the preceding test are gone.
  assert(!ActiveReferences);
  AddRefs = Releases = Destructions = MethodCalls = 0;
  struct ForeignCOMObject *object = malloc(sizeof(*object));
  assert(object);
  object->Value = (struct Interface){&ValueVTable, object};
  object->Property = (struct Interface){&PropertyVTable, object};
  object->ValueInterface = &object->Value;
  object->PropertyInterface = &object->Property;
  object->References = 1;
  object->StoredValue = value;
  ActiveReferences = 1;
  return object;
}

const void *
ForeignCOMObject_GetValueStorage(const struct ForeignCOMObject *object) {
  return &object->ValueInterface;
}

const void *
ForeignCOMObject_GetPropertyStorage(const struct ForeignCOMObject *object) {
  return &object->PropertyInterface;
}

uint32_t ForeignCOMObject_Release(struct ForeignCOMObject *object) {
  return Release(&object->Value);
}

uint32_t GetForeignCOMReferenceCount(void) { return ActiveReferences; }
uint32_t GetForeignCOMAddRefCalls(void) { return AddRefs; }
uint32_t GetForeignCOMReleaseCalls(void) { return Releases; }
uint32_t GetForeignCOMDestructionCount(void) { return Destructions; }
uint32_t GetForeignCOMMethodCalls(void) { return MethodCalls; }
