#ifndef IMPORT_AS_MEMBER_B_H
#define IMPORT_AS_MEMBER_B_H

#include "ImportAsMember.h"


extern double IAMStruct1StaticVar1
     __attribute__((swift_name("Struct1.static1")));
extern float IAMStruct1StaticVar2
     __attribute__((swift_name("Struct1.static2")));

extern struct IAMStruct1 IAMStruct1CreateFloat(float value)
    __attribute__((swift_name("Struct1.init(float:)")));

#endif // IMPORT_AS_MEMBER_B_H
