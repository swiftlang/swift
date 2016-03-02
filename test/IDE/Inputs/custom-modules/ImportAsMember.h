#ifndef IMPORT_AS_MEMBER_H
#define IMPORT_AS_MEMBER_H

struct __attribute__((swift_name("Struct1"))) IAMStruct1 {
  double x, y, z;
};

struct IAMStruct1 IAMStruct1CreateSimple(int value) __attribute__((swift_name("Struct1.init(value:)")));

double IAMStruct1GetRadius(struct IAMStruct1 s) __attribute__((swift_name("getter:Struct1.radius(self:)")));

void IAMStruct1SetRadius(struct IAMStruct1 s, double radius) __attribute__((swift_name("setter:Struct1.radius(self:_:)")));

#endif // IMPORT_AS_MEMBER_H
