// This is for exercising naming collisions with other modules

#include "InferImportAsMember.h"

extern double IAMStruct1GetNonPropertyExternalCollision(struct IAMStruct1 s);

extern void IAMStruct1SetCollisionNonProperty(struct IAMStruct1, int, float);

