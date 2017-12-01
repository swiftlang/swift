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


