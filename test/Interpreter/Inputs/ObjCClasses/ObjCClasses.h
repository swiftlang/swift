#ifndef SWIFT_TEST_OBJC_CLASSES_H
#define SWIFT_TEST_OBJC_CLASSES_H

#include <Foundation/NSObject.h>

/* This class has instance variables which are not apparent in the
   interface.  Subclasses will need to be slid by the ObjC runtime. */
@interface HasHiddenIvars : NSObject
@property NSInteger x;
@property NSInteger y;
@property NSInteger z;
@property NSInteger t;
@end

/* This class has a method that doesn't fill in the error properly. */
@interface NilError : NSObject
+ (BOOL) throwIt: (NSError**) error;
@end


#if __has_feature(objc_class_property)
@protocol ProtoWithClassProperty
+ (void)reset;
@property (class) int value;

@optional
@property (class, readonly) BOOL optionalClassProp;
@end

@interface ClassWithClassProperty : NSObject <ProtoWithClassProperty>
@end

@interface ObjCSubclassWithClassProperty : ClassWithClassProperty
// Deliberately redeclared.
@property (class) int value;
@end

@protocol PropertyNamingConflictProto
@property (nullable) id protoProp;
@property (class, nullable) id protoProp;
@end

@interface PropertyNamingConflict : NSObject
@property (readonly, nullable) id prop;
@property (class, readonly, nullable) id prop;
@end

#endif // __has_feature(objc_class_property)

#endif
