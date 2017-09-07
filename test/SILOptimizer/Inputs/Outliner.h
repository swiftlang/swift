#import <Foundation/Foundation.h>

@interface Gizmo : NSObject
@property (nonatomic)NSString *stringProperty;
- (NSString*) modifyString: (NSString *)str withNumber: (NSInteger) num withFoobar: (id)foobar;
- (id) doSomething : (NSArray<NSString*>*) arr;
@end


