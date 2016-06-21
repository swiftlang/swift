#ifndef IMPORT_AS_MEMBER_B_H
#define IMPORT_AS_MEMBER_B_H

#include "ImportAsMember.h"


extern double IAMStruct1StaticVar1
     __attribute__((swift_name("Struct1.static1")));
extern float IAMStruct1StaticVar2
     __attribute__((swift_name("Struct1.static2")));

extern struct IAMStruct1 IAMStruct1CreateFloat(float value)
    __attribute__((swift_name("Struct1.init(float:)")));

struct IAMStruct1 IAMStruct1GetCurrentStruct1(void)
  __attribute__((swift_name("getter:currentStruct1()")));

void IAMStruct1SetCurrentStruct1(struct IAMStruct1 newValue)
  __attribute__((swift_name("setter:currentStruct1(_:)")));

struct IAMStruct1 IAMStruct1GetZeroStruct1(void)
  __attribute__((swift_name("getter:Struct1.zero()")));

#endif // IMPORT_AS_MEMBER_B_H
