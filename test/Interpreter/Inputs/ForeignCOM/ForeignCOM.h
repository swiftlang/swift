#ifndef FOREIGN_COM_H
#define FOREIGN_COM_H

#include <stdint.h>

struct ForeignCOMObject;

// Create returns an owned reference to the object. Each storage accessor
// borrows an interface pointer; loading it as a Swift existential copies it.
struct ForeignCOMObject *ForeignCOMObject_Create(int32_t value);
const void *
ForeignCOMObject_GetValueStorage(const struct ForeignCOMObject *object);
const void *
ForeignCOMObject_GetPropertyStorage(const struct ForeignCOMObject *object);
uint32_t ForeignCOMObject_Release(struct ForeignCOMObject *object);

// Fixture counters remain observable after the object is destroyed.
uint32_t GetForeignCOMReferenceCount(void);
uint32_t GetForeignCOMAddRefCalls(void);
uint32_t GetForeignCOMReleaseCalls(void);
uint32_t GetForeignCOMDestructionCount(void);
uint32_t GetForeignCOMMethodCalls(void);
uint32_t GetForeignCOMQueryInterfaceCalls(void);

#endif
