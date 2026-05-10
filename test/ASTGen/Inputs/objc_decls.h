@import Foundation;

@interface ObjCClass: NSObject
@property NSString *theValue;
-(void)methodWithX:(NSInteger)x Y:(NSInteger)y;
@end

@interface ObjCClass(Category1)
@end
@interface ObjCClass(Category2)
@end

@interface ObjCClass2: NSObject

@end
