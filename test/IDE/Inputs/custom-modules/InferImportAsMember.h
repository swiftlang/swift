#ifndef INFER_IMPORT_AS_MEMBER_H
#define INFER_IMPORT_AS_MEMBER_H

struct IAMStruct1 {
  double x, y, z;
};

extern double IAMStruct1GlobalVar;

// Init
extern struct IAMStruct1 IAMStruct1CreateCopy(struct IAMStruct1 in);
extern struct IAMStruct1 IAMStruct1CreateSimple(double value);

// Methods
extern struct IAMStruct1 IAMStruct1Invert(struct IAMStruct1 s);
extern void IAMStruct1InvertInPlace(struct IAMStruct1 *s);
extern struct IAMStruct1 IAMStruct1Rotate(const struct IAMStruct1 *s,
                                          double radians);

// Properties
extern double IAMStruct1GetRadius(struct IAMStruct1 s);
extern void IAMStruct1SetRadius(struct IAMStruct1 s, double radius);
extern double IAMStruct1GetAltitude(struct IAMStruct1 s);
extern void IAMStruct1SetAltitude(struct IAMStruct1 *s, double altitude);
extern double IAMStruct1GetMagnitude(struct IAMStruct1 s);

// We can't do a property for this one, too many arguments to set
extern int IAMStruct1GetNonProperty(struct IAMStruct1 s);
extern void IAMStruct1SetNonProperty(struct IAMStruct1 s, int a, int b);

extern void IAMStruct1SelfComesLast(double x, struct IAMStruct1 s);
extern void IAMStruct1SelfComesThird(int a, float b, struct IAMStruct1 s,
                                     double x);

// Static method
extern int IAMStruct1StaticMethod();

// typedef __attribute__((objc_bridge(id))) void *IAMClassRef;
struct IAMClass {
  int x, y, z;
};
typedef struct IAMClass *IAMClassRef;

extern unsigned IAMClassGetTypeID();

extern IAMClassRef IAMClassCreate(int i);

#endif // INFER_IMPORT_AS_MEMBER_H
