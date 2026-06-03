#import <Foundation/Foundation.h>

@class Bar;
@class Baz;
@protocol FooDelegate;

NS_ASSUME_NONNULL_BEGIN

@interface Foo: NSObject
@property (weak, nullable) id <FooDelegate> delegate;
- (NSString *)doSomething;
@end

@protocol FooDelegate <NSObject>
- (NSString *)takingObject:(NSObject *)NS_SWIFT_SENDING object;

- (NSObject * NS_SWIFT_SENDING)identityFn:(NSObject *)NS_SWIFT_SENDING object;
@end

NS_ASSUME_NONNULL_END

// ----------------------------------------------------
// From https://github.com/swiftlang/swift/issues/87659

@class Payload;

@interface Payload : NSObject
@property (nonatomic, copy) NSString *name;
@end

NS_SWIFT_SENDABLE
@protocol Handler <NSObject>
- (void)handle:(NS_SWIFT_SENDING Payload *)value;
@end

@interface LegacyObjCHandler : NSObject <Handler>
@end
