#import <Foundation/Foundation.h>

@protocol Proto
- (id)requirement;
@end

@interface Gizmo : NSObject
@property (nonatomic)NSString *stringProperty;
- (NSString*) modifyString: (NSString *)str withNumber: (NSInteger) num withFoobar: (id)foobar;
- (id) doSomething : (NSArray<NSString*>*) arr;
@end

@interface Gizmo2<ObjectType: id<Proto>> : NSObject
- (NSString*) doSomething;
@end

@protocol FooProto <NSObject>
@end

@protocol SomeGenericClass <FooProto>
@property (nonatomic, nullable, readonly, strong) NSString *version;
- (NSString*) doSomething;
- (id) doSomething2 : (NSArray<NSString*>*) arr;
@end

typedef NS_ENUM(NSUInteger, MyEventType) {
    MyEventTypeA = 1,
    MyEventTypeB = 2
};

@interface MyWindow : NSObject
@property NSInteger windowNumber;
@end

@interface MyView : NSObject
@property (nonatomic, nullable, readonly, strong) MyWindow *window;
@end

typedef struct MyPoint {
NSInteger x;
NSInteger y;
} MyPoint;

@interface MyGraphicsContext : NSObject
@end

@interface MyEvent : NSObject
+ (nullable MyEvent *)mouseEventWithType:(MyEventType)type
                                location:(MyPoint)pt
                            windowNumber:(NSInteger)wNum
                                 context:(nullable MyGraphicsContext * __unused)context
                             eventNumber:(NSInteger)eNum
                              clickCount:(NSInteger)cnt
                                pressure:(float)pressure;
@end

NS_ASSUME_NONNULL_BEGIN
@protocol Treeish <NSObject>
- (nullable NSArray *) treeishChildren;
@end
NS_ASSUME_NONNULL_END
