#ifndef INFER_IMPORT_AS_MEMBER_H
#define INFER_IMPORT_AS_MEMBER_H

struct IAMStruct1 {
  double x, y, z;
};

extern double IAMStruct1GlobalVar;

/// Init
extern struct IAMStruct1 IAMStruct1CreateCopy(struct IAMStruct1 in);
extern struct IAMStruct1 IAMStruct1CreateSimple(double value);
extern struct IAMStruct1 IAMStruct1CreateRedundant(double redundant);
extern struct IAMStruct1 IAMStruct1CreateSpecialLabel();

/// Methods
extern struct IAMStruct1 IAMStruct1Invert(struct IAMStruct1 s);
extern void IAMStruct1InvertInPlace(struct IAMStruct1 *s);
extern struct IAMStruct1 IAMStruct1Rotate(const struct IAMStruct1 *s,
                                          double radians);
extern void IAMStruct1SelfComesLast(double x, struct IAMStruct1 s);
extern void IAMStruct1SelfComesThird(double a, float b, struct IAMStruct1 s,
                                     double x);

/// Properties
extern double IAMStruct1GetRadius(struct IAMStruct1 s);
extern void IAMStruct1SetRadius(struct IAMStruct1 s, double radius);
extern double IAMStruct1GetAltitude(struct IAMStruct1 s);
extern void IAMStruct1SetAltitude(struct IAMStruct1 *s, double altitude);
extern double IAMStruct1GetMagnitude(struct IAMStruct1 s);
extern void IAMStruct1SetLength(double len, struct IAMStruct1 *s);
extern double IAMStruct1GetLength(struct IAMStruct1 s);


/// Various instance functions that can't quite be imported as properties.

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
extern void IAMStruct1SetNonPropertyExternalCollision(struct IAMStruct1 s, double x);

/// Various static functions that can't quite be imported as properties.
// Too many parameters
extern float IAMStruct1StaticGetNonPropertyNumParams(void);
extern void IAMStruct1StaticSetNonPropertyNumParams(float a,
                                              float b);
extern void IAMStruct1StaticGetNonPropertyNumParamsGetter(double d);

// Set type doesn't match get type
extern float IAMStruct1StaticGetNonPropertyType(void);
extern void IAMStruct1StaticSetNonPropertyType(double x);

// Didn't find self on setter
extern float IAMStruct1StaticGetNonPropertyNoSelf(void);
extern void IAMStruct1StaticSetNonPropertyNoSelf(double x, double y);

// No set only properties
extern void IAMStruct1StaticSetNonPropertyNoGet(double x);

/// Static method
extern double IAMStruct1StaticMethod(void);
extern double IAMStruct1TLAThreeLetterAcronym(void);

/// Static computed properties
extern double IAMStruct1StaticGetProperty(void);
extern double IAMStruct1StaticSetProperty(double);
extern double IAMStruct1StaticGetOnlyProperty(void);

/// Omit needless words
extern double IAMStruct1ONWNeedlessTypeArgLabel(double Double);

/// Fuzzy
extern struct IAMStruct1 IAMFuzzyStruct1Create(void);
extern struct IAMStruct1 IAMFuzzyStruct1CreateWithFuzzyName(void);
extern struct IAMStruct1 IAMFuzzyStruct1CreateFuzzyName(void);

extern double __IAMStruct1IgnoreMe(struct IAMStruct1 s);

/// Mutable
struct IAMMutableStruct1 {};
struct IAMMutableStruct1
IAMStruct1CreateMutable(struct IAMStruct1 withIAMStruct1);
struct IAMMutableStruct1 IAMStruct1CreateMutableWithURL(const char *url);
static void IAMStruct1DoSomething(struct IAMMutableStruct1 iamStruct1);

typedef struct TDStruct TDStruct;
struct TDStruct {
  double x;
};
// FIXME: the below doesn't appear in the output
const TDStruct TDStructCreateWithFloat(float Float);

/// Class
typedef const struct __attribute__((objc_bridge(id))) __IAMClass *IAMClassRef;
typedef IAMClassRef IAMOtherName;

extern unsigned IAMClassGetTypeID(void);
extern IAMClassRef IAMClassCreate(double i);
extern void IAMClassInvert(IAMOtherName iamOtherName);

// Test collision where we can see the getter, but not setter
extern float IAMStruct1GetCollisionNonProperty(struct IAMStruct1, int);

/// Struct with pointer properties and a synthesized memberwise init.
typedef struct IAMStruct2 IAMStruct2;
typedef struct IAMStruct2 {
  double *ptr1;
  double *ptr2;
};

extern struct IAMStruct2 IAMStruct2CreateOther(double *ptr);

#endif // INFER_IMPORT_AS_MEMBER_H
