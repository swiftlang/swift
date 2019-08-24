@import Foundation;

#pragma clang assume_nonnull begin

// stdbool.h uses #define, so this test does as well.
#ifndef bool
# define bool _Bool
#endif

bool testCBool(bool);
BOOL testObjCBool(BOOL);
Boolean testDarwinBoolean(Boolean);

typedef bool CBoolTypedef;
typedef BOOL ObjCBoolTypedef;
typedef Boolean DarwinBooleanTypedef;

CBoolTypedef testCBoolTypedef(CBoolTypedef);
ObjCBoolTypedef testObjCBoolTypedef(ObjCBoolTypedef);
DarwinBooleanTypedef testDarwinBooleanTypedef(DarwinBooleanTypedef);

const bool *testCBoolPointer(bool *);
const BOOL *testObjCBoolPointer(BOOL *);
const Boolean *testDarwinBooleanPointer(Boolean *);

typedef bool (*CBoolFn)(bool);
typedef BOOL (*ObjCBoolFn)(BOOL);
typedef Boolean (*DarwinBooleanFn)(Boolean);

typedef bool (^CBoolBlock)(bool);
typedef BOOL (^ObjCBoolBlock)(BOOL);
typedef Boolean (^DarwinBooleanBlock)(Boolean);

__typeof(bool (^)(bool)) testCBoolFnToBlock(bool (*)(bool));
__typeof(BOOL (^)(BOOL)) testObjCBoolFnToBlock(BOOL (*)(BOOL));
__typeof(Boolean (^)(Boolean)) testDarwinBooleanFnToBlock(Boolean (*)(Boolean));

CBoolBlock testCBoolFnToBlockTypedef(CBoolFn);
ObjCBoolBlock testObjCBoolFnToBlockTypedef(ObjCBoolFn);
DarwinBooleanBlock testDarwinBooleanFnToBlockTypedef(DarwinBooleanFn);

typedef __typeof(testCBoolFnToBlockTypedef) CBoolFnToBlockType;
typedef __typeof(testObjCBoolFnToBlockTypedef) ObjCCBoolFnToBlockType;
typedef __typeof(testDarwinBooleanFnToBlockTypedef) DarwinBooleanFnToBlockType;

extern CBoolFn globalCBoolFn;
extern ObjCBoolFn globalObjCBoolFn;
extern DarwinBooleanFn globalDarwinBooleanFn;

extern CBoolBlock globalCBoolBlock;
extern ObjCBoolBlock globalObjCBoolBlock;
extern DarwinBooleanBlock globalDarwinBooleanBlock;

@interface Test : NSObject
@property bool propCBool;
@property BOOL propObjCBool;
@property Boolean propDarwinBoolean;

- (bool)testCBool:(bool)b;
- (BOOL)testObjCBool:(BOOL)b;
- (Boolean)testDarwinBoolean:(Boolean)b;

@property bool (^propCBoolBlock)(bool);
@property BOOL (^propObjCBoolBlock)(BOOL);
@property Boolean (^propDarwinBooleanBlock)(Boolean);

- (bool (^)(bool))testCBoolFnToBlock:(bool (*)(bool))fp;
- (BOOL (^)(BOOL))testObjCBoolFnToBlock:(BOOL (*)(BOOL))fp;
- (Boolean (^)(Boolean))testDarwinBooleanFnToBlock:(Boolean (*)(Boolean))fp;

- (void)produceCBoolBlockTypedef:(CBoolBlock _Nullable *_Nonnull)outBlock;
- (void)produceObjCBoolBlockTypedef:(ObjCBoolBlock _Nullable *_Nonnull)outBlock;
- (void)produceDarwinBooleanBlockTypedef:
    (DarwinBooleanBlock _Nullable *_Nonnull)outBlock;

- (instancetype)init;
@end

#pragma clang assume_nonnull end
