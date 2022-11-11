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

// FIXME: test case involving swift_name

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

// FIXME: test case involving swift_name

@property int categoryPropertyFromHeader1;
@property int categoryPropertyFromHeader2;
@property int categoryPropertyFromHeader3;
@property int categoryPropertyFromHeader4;

@end

@interface ObjCSubclass : ObjCClass

- (void)subclassMethodFromHeader1:(int)param;

@end

struct ObjCStruct {
  int foo;
};
