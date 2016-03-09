#ifndef INFER_IMPORT_AS_MEMBER_H
#define INFER_IMPORT_AS_MEMBER_H

struct IAMStruct1 {
  double x, y, z;
};

extern double IAMStruct1GlobalVar;

/// Init
extern struct IAMStruct1 IAMStruct1CreateCopy(struct IAMStruct1 in);
extern struct IAMStruct1 IAMStruct1CreateSimple(double value);

/// Methods
extern struct IAMStruct1 IAMStruct1Invert(struct IAMStruct1 s);
extern void IAMStruct1InvertInPlace(struct IAMStruct1 *s);
extern struct IAMStruct1 IAMStruct1Rotate(const struct IAMStruct1 *s,
                                          double radians);
extern void IAMStruct1SelfComesLast(double x, struct IAMStruct1 s);
extern void IAMStruct1SelfComesThird(int a, float b, struct IAMStruct1 s,
                                     double x);

/// Properties
extern double IAMStruct1GetRadius(struct IAMStruct1 s);
extern void IAMStruct1SetRadius(struct IAMStruct1 s, double radius);
extern double IAMStruct1GetAltitude(struct IAMStruct1 s);
extern void IAMStruct1SetAltitude(struct IAMStruct1 *s, double altitude);
extern double IAMStruct1GetMagnitude(struct IAMStruct1 s);
extern void IAMStruct1SetLength(double len, struct IAMStruct1 *s);
extern double IAMStruct1GetLength(struct IAMStruct1 s);


/// Various functions that can't quite be imported as properties.

// Too many parameters in the setter
extern float IAMStruct1GetNonPropertyNumParams(struct IAMStruct1 s);
extern void IAMStruct1SetNonPropertyNumParams(struct IAMStruct1 s, float a,
                                              float b);

// Set type doesn't match get type
extern float IAMStruct1GetNonPropertyType(struct IAMStruct1 s);
extern void IAMStruct1SetNonPropertyType(struct IAMStruct1 s, double x);

// Didn't find self on setter
extern float IAMStruct1GetNonPropertyNoSelf(struct IAMStruct1 s);
extern void IAMStruct1SetNonPropertyNoSelf(double x, double y);

// No set only properties
extern void IAMStruct1SetNonPropertyNoGet(struct IAMStruct1 s, double x);

// Static versions
// Too many parameters in the setter
extern float IAMStruct1StaticGetNonPropertyNumParams();
extern void IAMStruct1StaticSetNonPropertyNumParams(float a,
                                              float b);

// Set type doesn't match get type
extern float IAMStruct1StaticGetNonPropertyType();
extern void IAMStruct1StaticSetNonPropertyType(double x);

// Didn't find self on setter
extern float IAMStruct1StaticGetNonPropertyNoSelf();
extern void IAMStruct1StaticSetNonPropertyNoSelf(double x, double y);

// No set only properties
extern void IAMStruct1StaticSetNonPropertyNoGet(double x);


/// Static method
extern int IAMStruct1StaticMethod();

/// Static computed properties
extern int IAMStruct1StaticGetProperty();
extern int IAMStruct1StaticSetProperty(int i);
extern int IAMStruct1StaticGetOnlyProperty();

// typedef __attribute__((objc_bridge(id))) void *IAMClassRef;
struct IAMClass {
  int x, y, z;
};
typedef struct IAMClass *IAMClassRef;

extern unsigned IAMClassGetTypeID();

extern IAMClassRef IAMClassCreate(int i);

#endif // INFER_IMPORT_AS_MEMBER_H
