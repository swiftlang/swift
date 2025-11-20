#ifndef IMPORT_AS_MEMBER_H
#define IMPORT_AS_MEMBER_H

struct __attribute__((swift_name("Struct1"))) IAMStruct1 {
  double x, y, z;
};

extern double IAMStruct1GlobalVar
    __attribute__((swift_name("Struct1.globalVar")));

extern struct IAMStruct1 IAMStruct1CreateSimple(double value)
    __attribute__((swift_name("Struct1.init(value:)")));

extern struct IAMStruct1 IAMStruct1CreateSpecialLabel(void)
    __attribute__((swift_name("Struct1.init(specialLabel:)")));

extern struct IAMStruct1 IAMStruct1CreateFull(double x, double y, double z)
    __attribute__((swift_name("Struct1.init(x:y:z:)")));

extern struct IAMStruct1 IAMStruct1Invert(struct IAMStruct1 s)
    __attribute__((swift_name("Struct1.inverted(self:)")));

extern void IAMStruct1InvertInPlace(struct IAMStruct1 *s)
    __attribute__((swift_name("Struct1.invert(self:)")));

extern struct IAMStruct1 IAMStruct1Rotate(const struct IAMStruct1 *s,
                                          double radians)
    __attribute__((swift_name("Struct1.translate(self:radians:)")));

extern struct IAMStruct1 IAMStruct1Scale(struct IAMStruct1 s,
                                         double radians)
    __attribute__((swift_name("Struct1.scale(self:_:)")));

extern double IAMStruct1GetRadius(const struct IAMStruct1 *s)
    __attribute__((swift_name("getter:Struct1.radius(self:)")));

extern void IAMStruct1SetRadius(struct IAMStruct1 s, double radius)
    __attribute__((swift_name("setter:Struct1.radius(self:_:)")));

extern double IAMStruct1GetAltitude(struct IAMStruct1 s)
    __attribute__((swift_name("getter:Struct1.altitude(self:)")));

extern void IAMStruct1SetAltitude(struct IAMStruct1 *s, double altitude)
    __attribute__((swift_name("setter:Struct1.altitude(self:_:)")));

extern double IAMStruct1GetMagnitude(struct IAMStruct1 s)
    __attribute__((swift_name("getter:Struct1.magnitude(self:)")));

extern int IAMStruct1StaticMethod(void)
    __attribute__((swift_name("Struct1.staticMethod()")));
extern int IAMStruct1StaticGetProperty(void)
    __attribute__((swift_name("getter:Struct1.property()")));
extern int IAMStruct1StaticSetProperty(int i)
    __attribute__((swift_name("setter:Struct1.property(i:)")));
extern int IAMStruct1StaticGetOnlyProperty(void)
    __attribute__((swift_name("getter:Struct1.getOnlyProperty()")));

extern void IAMStruct1SelfComesLast(double x, struct IAMStruct1 s)
    __attribute__((swift_name("Struct1.selfComesLast(x:self:)")));
extern void IAMStruct1SelfComesThird(int a, float b, struct IAMStruct1 s, double x)
    __attribute__((swift_name("Struct1.selfComesThird(a:b:self:x:)")));


struct IAMMultipleNested {
  int value;
};

typedef int MNInnerInt __attribute__((swift_name("IAMMultipleNested.Inner")));
typedef float MNInnerFloat __attribute__((swift_name("IAMMultipleNested.Inner")));

typedef int IAMBadInnerInt
    __attribute__((swift_name("IAMNonexistent.Inner")));
// CHECK: ImportAsMember.h:[[@LINE-1]]:{{[0-9]+}}: warning: imported declaration 'IAMBadInnerInt' could not be mapped to 'IAMNonexistent.Inner' [#ClangDeclarationImport]
// CHECK: note: please report this issue to the owners of 'ImportAsMember' [#ClangDeclarationImport]
typedef int IAMBadInnerIntAPINotes;
// CHECK: ImportAsMember.h:[[@LINE-1]]:{{[0-9]+}}: warning: imported declaration 'IAMBadInnerIntAPINotes' could not be mapped to 'IAMNonexistent.Inner2'
// CHECK: note: please report this issue to the owners of 'ImportAsMember'

@interface IAMPrivateParent @end
@interface IAMPrivateChild
- (instancetype)init;
@end

#endif // IMPORT_AS_MEMBER_H
