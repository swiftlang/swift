#if __OBJC__

@import Foundation;

@interface ObjCBaseClass

- (instancetype)init __attribute__((unavailable));

// Need two initializers to reproduce certain conflict bugs.
- (instancetype)initFromSuperclass:(int)param  __attribute__((objc_designated_initializer));
- (instancetype)initFromSuperclass2:(int)param  __attribute__((objc_designated_initializer));

- (void)superclassMethod:(int)param;
@property (assign) int superclassProperty;

+ (void)superclassClassMethod;

@end

@protocol ObjCProto

- (instancetype)initFromProtocol1:(int)param;
- (instancetype)initFromProtocol2:(int)param;

@end

@interface ObjCClass : ObjCBaseClass <ObjCProto>

- (instancetype)initNotFromProtocol:(int)param;

- (void)methodFromHeader1:(int)param;
- (void)methodFromHeader2:(int)param;
- (void)methodFromHeader3:(int)param;
- (void)methodFromHeader4:(int)param;
- (int)methodFromHeader5;
- (void)methodFromHeader6:(int)param;

@property int propertyFromHeader1;
@property int propertyFromHeader2;
@property int propertyFromHeader3;
@property int propertyFromHeader4;
@property int propertyFromHeader5;
@property int propertyFromHeader6;
@property int propertyFromHeader7;
@property int propertyFromHeader8;
@property int propertyFromHeader9;
@property int propertyFromHeader10;
@property int propertyFromHeader11;

@property (readonly) int readonlyPropertyFromHeader1;
@property (readonly) int readonlyPropertyFromHeader2;
@property (readonly) int readonlyPropertyFromHeader3;
@property (readonly) int readonlyPropertyFromHeader4;
@property (readonly) int readonlyPropertyFromHeader5;
@property (readonly) int readonlyPropertyFromHeader6;

+ (void)classMethod1:(int)param;
+ (void)classMethod2:(int)param;
+ (void)classMethod3:(int)param;

- (void)instanceMethod1:(int)param;
- (void)instanceMethod2:(int)param;

// rdar://122280735 - crash when the parameter of a block property needs @escaping
@property (nonatomic, readonly) void (^ _Nonnull rdar122280735)(void (^_Nonnull completion)());

@end

@interface ObjCClass () <NSCopying>

- (void)extensionMethodFromHeader1:(int)param;
- (void)extensionMethodFromHeader2:(int)param;

@end

@interface ObjCClass (PresentAdditions)

- (void)categoryMethodFromHeader1:(int)param;
- (void)categoryMethodFromHeader2:(int)param;
- (void)categoryMethodFromHeader3:(int)param;
- (void)categoryMethodFromHeader4:(int)param;

@property int categoryPropertyFromHeader1;
@property int categoryPropertyFromHeader2;
@property int categoryPropertyFromHeader3;
@property int categoryPropertyFromHeader4;

@end

@interface ObjCClass (SwiftNameTests)

- (void)methodObjCName1 __attribute__((swift_name("methodSwiftName1()")));
- (void)methodObjCName2 __attribute__((swift_name("methodSwiftName2()")));
- (void)methodObjCName3 __attribute__((swift_name("methodSwiftName3()")));
- (void)methodObjCName4 __attribute__((swift_name("methodSwiftName4()")));
- (void)methodObjCName5 __attribute__((swift_name("methodSwiftName5()")));
- (void)methodObjCName6A __attribute__((swift_name("methodSwiftName6A()")));
- (void)methodObjCName6B __attribute__((swift_name("methodSwiftName6B()")));

@end

@interface ObjCClass (AmbiguousMethods)

- (void)ambiguousMethod1WithCInt:(int)param  __attribute__((swift_name("ambiguousMethod1(with:)")));
- (void)ambiguousMethod1WithCChar:(char)param  __attribute__((swift_name("ambiguousMethod1(with:)")));

- (void)ambiguousMethod2WithCInt:(int)param  __attribute__((swift_name("ambiguousMethod2(with:)")));
- (void)ambiguousMethod2WithCChar:(char)param  __attribute__((swift_name("ambiguousMethod2(with:)")));

- (void)ambiguousMethod3WithCInt:(int)param  __attribute__((swift_name("ambiguousMethod3(with:)")));
- (void)ambiguousMethod3WithCChar:(char)param  __attribute__((swift_name("ambiguousMethod3(with:)")));

- (void)ambiguousMethod4WithCInt:(int)param  __attribute__((swift_name("ambiguousMethod4(with:)")));

@end

@interface ObjCClass (Effects)
- (void)doSomethingAsynchronousWithCompletionHandler:(void (^ _Nonnull)(id _Nullable result, NSError * _Nullable error))completionHandler;
- (void)doSomethingElseAsynchronousWithCompletionHandler:(void (^ _Nullable)(id _Nonnull result))completionHandler;
- (void)doSomethingFunAndAsynchronousWithCompletionHandler:(void (^ _Nonnull)(id _Nullable result, NSError * _Nullable error))completionHandler;

- (void)doSomethingOverloadedWithCompletionHandler:(void (^ _Nonnull)())completionHandler;
- (void)doSomethingOverloaded __attribute__((__swift_attr__("@_unavailableFromAsync(message: \"Use async doSomethingOverloaded instead.\")")));

- (BOOL)doSomethingThatCanFailWithHandler:(void (^ _Nonnull)())handler error:(NSError **)error;
- (BOOL)doSomethingElseThatCanFail:(NSError **)error handler:(void (^ _Nonnull)())handler;
- (BOOL)doSomethingThatCanFailWithWeirdParameterWithHandler:(void (^ _Nonnull)())handler :(NSError **)error;
- (int)doSomethingThatCanFailWithWeirdReturnCodeWithError:(NSError **)error __attribute__((swift_error(nonzero_result)));

@end

@interface ObjCClass (InvalidMembers)

- (void)unimplementedMember;
- (void)nonObjCMethod:(id)value;

@end

@interface ObjCClass (EmptyCategory)
@end

@protocol PartiallyOptionalProtocol

- (void)requiredMethod1;
- (void)requiredMethod2;

@optional
- (void)optionalMethod1;
- (void)optionalMethod2;

@end

@interface ObjCClass (Conformance) <PartiallyOptionalProtocol>

@end

@interface ObjCClass (TypeMatchOptionality)

- (nullable id)nullableResultAndArg:(nullable id)arg;
- (nonnull id)nonnullResultAndArg:(nonnull id)arg;
- (null_unspecified id)nullUnspecifiedResultAndArg:(null_unspecified id)arg;

- (nonnull id)nonnullResult1;
- (nonnull id)nonnullResult2;
- (nonnull id)nonnullResult3;

- (void)nonnullArgument1:(nonnull id)arg;
- (void)nonnullArgument2:(nonnull id)arg;
- (void)nonnullArgument3:(nonnull id)arg;

- (nullable id)nullableResult;
- (void)nullableArgument:(nullable id)arg;

@end

@interface ObjCClass (TypeMatchOptionalityInvalid)

- (int)nonPointerResult;
- (void)nonPointerArgument:(int)arg;

@end

@interface ObjCSubclass : ObjCClass

- (void)subclassMethodFromHeader1:(int)param;

@end

@interface ObjCImplSubclass : ObjCClass

@end

@interface ObjCBasicInitClass : ObjCBaseClass

- (nonnull instancetype)init __attribute__((objc_designated_initializer));

@end

@interface ObjCImplRootClass

@end

@interface ObjCImplGenericClass<T> : NSObject

@end

@interface ObjCBadClass : NSObject
@end

@interface ObjCBadClass (BadCategory1)
@end

@interface ObjCBadClass (BadCategory2)
@end

@protocol EmptyObjCProto
@end

@interface ObjCClassWithWeirdSwiftAttributeCombo : ObjCBaseClass

@end

#endif

void CImplFunc1(int param);
void CImplFunc2(int param);

void CImplFuncMismatch1(int param);
void CImplFuncMismatch2(int param);

#if __OBJC__
void CImplFuncMismatch3(_Nullable id param);
void CImplFuncMismatch4(_Nullable id param);
void CImplFuncMismatch5(_Nonnull id param);
void CImplFuncMismatch6(_Nonnull id param);
_Nullable id CImplFuncMismatch3a(int param);
_Nullable id CImplFuncMismatch4a(int param);
_Nonnull id CImplFuncMismatch5a(int param);
_Nonnull id CImplFuncMismatch6a(int param);
#endif

void CImplFuncNameMismatch1(int param);
void CImplFuncNameMismatch2(int param);

int CImplGetComputedGlobal1(void) __attribute__((swift_name("getter:cImplComputedGlobal1()")));
void CImplSetComputedGlobal1(int param) __attribute__((swift_name("setter:cImplComputedGlobal1(newValue:)")));

typedef struct CImplStruct {} CImplStruct;

void CImplStructStaticFunc1(int param) __attribute__((swift_name("CImplStruct.staticFunc1(_:)")));

struct ObjCStruct {
  int foo;
};
