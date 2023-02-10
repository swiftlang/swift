@interface ObjCBaseClass


// Need two initializers to reproduce certain conflict bugs.
- (instancetype)initFromSuperclass:(int)param  __attribute__((objc_designated_initializer));
- (instancetype)initFromSuperclass2:(int)param  __attribute__((objc_designated_initializer));

- (void)superclassMethod:(int)param;
@property (assign) int superclassProperty;

@end

@interface ObjCClass : ObjCBaseClass

- (void)methodFromHeader1:(int)param;
- (void)methodFromHeader2:(int)param;
- (void)methodFromHeader3:(int)param;
- (void)methodFromHeader4:(int)param;

@property int propertyFromHeader1;
@property int propertyFromHeader2;
@property int propertyFromHeader3;
@property int propertyFromHeader4;
@property int propertyFromHeader5;
@property int propertyFromHeader6;
@property int propertyFromHeader7;
@property int propertyFromHeader8;
@property int propertyFromHeader9;

@property (readonly) int readonlyPropertyFromHeader1;
@property (readonly) int readonlyPropertyFromHeader2;
@property (readonly) int readonlyPropertyFromHeader3;
@property (readonly) int readonlyPropertyFromHeader4;
@property (readonly) int readonlyPropertyFromHeader5;
@property (readonly) int readonlyPropertyFromHeader6;

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

@interface ObjCSubclass : ObjCClass

- (void)subclassMethodFromHeader1:(int)param;

@end

struct ObjCStruct {
  int foo;
};
