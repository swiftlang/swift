/*  Foo.h
  Copyright (c) 1815, Napoleon Bonaparte. All rights reserved.
*/
#if !defined(__FOO_H__)
#define __FOO_H__ 1

#import <FooSub/FooSub.h>
#import <FooHelper/FooHelper.h>

// Types.

// and stuff.
// Yo.

/// Aaa.  FooEnum1.  Bbb.
enum FooEnum1 {
  /// Aaa.  FooEnum1X.  Bbb.
  FooEnum1X
};
enum FooEnum2 { FooEnum2X, FooEnum2Y };
enum FooEnum3 { FooEnum3X = 10, FooEnum3Y = 20 };

#define CF_ENUM(_type, _name) enum _name : _type _name; enum _name : _type
#define CF_OPTIONS(_type, _name) enum _name : _type _name; enum _name : _type
#define NS_ENUM(_type, _name) CF_ENUM(_type, _name)
#define NS_OPTIONS(_type, _name) CF_OPTIONS(_type, _name)

/// Aaa.  FooComparisonResult.  Bbb.
typedef NS_ENUM(long, FooComparisonResult) {
  // This is ascending
  FooOrderedAscending = -1L,
  FooOrderedSame, // But this is the same.
  FooOrderedDescending
};

/// Aaa.  FooRuncingOptions.  Bbb.
typedef NS_OPTIONS(long, FooRuncingOptions) {
  // This is mince.
  FooRuncingEnableMince = 1,
  FooRuncingEnableQuince = 2, /* But this is quince */
};

struct FooStruct1 {
  int x;
  double y;
};
typedef struct FooStruct1 *FooStruct1Pointer;

typedef struct FooStruct2 {
  int x;
  double y;
} FooStructTypedef1;

typedef struct {
  int x;
  double y;
} FooStructTypedef2;

/// Aaa.  FooTypedef1.  Bbb.
typedef int FooTypedef1;

/// Aaa.  fooIntVar.  Bbb.
extern int fooIntVar;

/// Aaa.  fooFunc1.  Bbb.
int fooFunc1(int a);

int fooFunc1AnonymousParam(int);
int fooFunc3(int a, float b, double c, int *d);

/*
  Very good
  fooFuncWithBlock function.
*/
extern
void fooFuncWithBlock(int (^blk)(float x));

void fooFuncWithFunctionPointer(int (*fptr)(float x));

void fooFuncNoreturn1(void) __attribute__((noreturn));
_Noreturn void fooFuncNoreturn2(void);

/**
 * Aaa.  fooFuncWithComment1.  Bbb.
 * Ccc.
 *
 * Ddd.
 */
void fooFuncWithComment1(void);

/*!
  Aaa.  fooFuncWithComment2.  Bbb.
 */
void fooFuncWithComment2(void);

/**
 * Aaa.  fooFuncWithComment3.  Bbb.
 */
/**
 * Ccc.
 */
void fooFuncWithComment3(void);

/**
 * Aaa.  fooFuncWithComment4.  Bbb.
 */
/// Ddd.
void fooFuncWithComment4(void);

/// Aaa.  fooFuncWithComment5.  Bbb.
/// Ccc.
///
/// Ddd.
void fooFuncWithComment5(void);


/// Aaa.  redeclaredInMultipleModulesFunc1.  Bbb.
int redeclaredInMultipleModulesFunc1(int a);

/// Aaa.  FooProtocolBase.  Bbb.
@protocol FooProtocolBase

/// Aaa.  fooProtoFunc.  Bbb.
/// Ccc.
- (void)fooProtoFunc;

  /// Aaa.  fooProtoFuncWithExtraIndentation1.  Bbb.
  /// Ccc.
  - (void)fooProtoFuncWithExtraIndentation1;

  /**
   * Aaa.  fooProtoFuncWithExtraIndentation2.  Bbb.
   * Ccc.
   */
  - (void)fooProtoFuncWithExtraIndentation2;

+ (void)fooProtoClassFunc;

@property int fooProperty1;
@property (readwrite) int fooProperty2;
@property (readonly) int fooProperty3;
@end

@protocol FooProtocolDerived<FooProtocolBase>
@end
#define NS_REFINED_FOR_SWIFT __attribute__((swift_private))
@interface FooClassBase
- (void) fooBaseInstanceFunc0;
- (FooClassBase *) fooBaseInstanceFunc1:(id)anObject;
- (instancetype) init __attribute__((objc_designated_initializer));
- (instancetype) initWithFloat:(float)f;
- (void) fooBaseInstanceFuncOverridden;
@property (readonly) int hiddenProp NS_REFINED_FOR_SWIFT;
+ (void) fooBaseClassFunc0;
@end

/// Aaa.  FooClassDerived.  Bbb.
@interface FooClassDerived : FooClassBase<FooProtocolDerived> {
  int fooIntIvar;
}

@property int fooProperty1;
@property (readwrite) int fooProperty2;
@property (readonly) int fooProperty3;

/* Blah..
   for fooInstanceFunc0..
   blah blah.
*/
- (void) fooInstanceFunc0;
- (void) fooInstanceFunc1:(int)a;
- (void) fooInstanceFunc2:(int)a withB:(int)b;

- (void) fooBaseInstanceFuncOverridden;

+ (void) fooClassFunc0;
@end

@class BarForwardDeclaredClass;
enum BarforwardDeclaredEnum;
typedef int typedef_int_t;

/* FOO_MACRO_1 is the answer */
#define FOO_MACRO_1 0
#define FOO_MACRO_2 1
#define FOO_MACRO_3 (-1) // Don't use FOO_MACRO_3 on Saturdays.
#define FOO_MACRO_4 0xffffffffu
#define FOO_MACRO_5 0xffffffffffffffffull
#define FOO_MACRO_6 ((typedef_int_t) 42)
#define FOO_MACRO_7 ((typedef_int_t) -1)
#define FOO_MACRO_8 ((char) 8)
#define FOO_MACRO_9 ((int) 9)
#define FOO_MACRO_10 ((short) 10)
#define FOO_MACRO_11 ((long) 11)
#define FOO_MACRO_OR (FOO_MACRO_2 | FOO_MACRO_6)
#define FOO_MACRO_AND (FOO_MACRO_2 & FOO_MACRO_6)
#define FOO_MACRO_BITWIDTH (FOO_MACRO_4 & FOO_MACRO_5)
#define FOO_MACRO_SIGNED (FOO_MACRO_2 & FOO_MACRO_4)
#define FOO_MACRO_TYPEDEF ((FooStruct1Pointer) 1)

#define FOO_MACRO_UNDEF_1 0
#undef FOO_MACRO_UNDEF_1

#define FOO_MACRO_REDEF_1 0
#undef FOO_MACRO_REDEF_1
#define FOO_MACRO_REDEF_1 1

#define FOO_MACRO_REDEF_2 0
#define FOO_MACRO_REDEF_2 1

#define FOO_MACRO_NOT_IMPORTED_1 10_9

void theLastDeclInFoo();

void _internalTopLevelFunc();

struct _InternalStruct {
  int x;
};

@interface FooClassBase(Cat1)
-(id) _internalMeth1;
@end

/* Extending FooClassBase with cool stuff */
@interface FooClassBase(Cat2)
-(id) _internalMeth2;
-(id) nonInternalMeth;
@end

@interface FooClassBase(Cat3)
-(id) _internalMeth3;
@end

@protocol _InternalProt
@end

@interface ClassWithInternalProt<_InternalProt>
@end

@interface FooClassPropertyOwnership : FooClassBase
@property (assign) id assignable;
@property (unsafe_unretained) id unsafeAssignable;
@property (retain) id retainable;
@property (strong) id strongRef;
@property (copy) id copyable;
@property (weak) id weakRef;
@property (assign) int scalar;
@end

#define FOO_NIL ((id)0)

@interface FooUnavailableMembers : FooClassBase
+ (instancetype)unavailableMembersWithInt:(int)i;

- (void)unavailable __attribute__((unavailable("x")));
- (void)swiftUnavailable __attribute__((annotate("swift1_unavailable")));
- (void)deprecated __attribute__((deprecated("x")));

- (void)availabilityIntroduced __attribute__((availability(macosx, introduced=10.1)));
- (void)availabilityDeprecated __attribute__((availability(macosx, deprecated=10.1)));
- (void)availabilityObsoleted __attribute__((availability(macosx, obsoleted=10.1)));
- (void)availabilityUnavailable __attribute__((availability(macosx, unavailable)));

- (void)availabilityIntroducedMsg __attribute__((availability(macosx, introduced=10.1, message="x")));
- (void)availabilityDeprecatedMsg __attribute__((availability(macosx, deprecated=10.1, message="x")));
- (void)availabilityObsoletedMsg __attribute__((availability(macosx, obsoleted=10.1, message="x")));
- (void)availabilityUnavailableMsg __attribute__((availability(macosx, unavailable, message="x")));
@end

typedef struct __attribute__((objc_bridge(id))) __FooCFType *FooCFTypeRef;
void FooCFTypeRelease(FooCFTypeRef);


#define __AVAILABILITY_INTERNAL_DEPRECATED_MSG(_msg)  __attribute__((deprecated(_msg)))
#define AB_DEPRECATED(msg) __AVAILABILITY_INTERNAL_DEPRECATED_MSG(msg)

typedef CF_ENUM(long, ABAuthorizationStatus) {
    kABAuthorizationStatusNotDetermined = 0,    // deprecated, use CNAuthorizationStatusNotDetermined
    kABAuthorizationStatusRestricted,           // deprecated, use CNAuthorizationStatusRestricted
} AB_DEPRECATED("use CNAuthorizationStatus");

#endif /* ! __FOO_H__ */
