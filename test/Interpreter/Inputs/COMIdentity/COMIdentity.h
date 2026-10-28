#ifndef COM_IDENTITY_H
#define COM_IDENTITY_H
#include <stdint.h>

struct COMIdentityObject;
struct COMIdentityObject *COMIdentityObject_Create(void *object,
                                                   const void *metadata,
                                                   void (*destroy)(void *),
                                                   int supportsIdentity);
const void *COMIdentityObject_GetStorage(struct COMIdentityObject *object);
uint32_t COMIdentityObject_Release(struct COMIdentityObject *object);
uint32_t COMIdentityObject_GetReferences(void);
uint32_t COMIdentityObject_GetQueries(void);
uint32_t COMIdentityObject_GetDestructions(void);
#endif
