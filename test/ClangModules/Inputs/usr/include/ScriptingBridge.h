@import Foundation;
@import ObjC;

@class NSString;

@interface SBElementArray : NSArray
- (id)objectWithName:(NSString *)name;
@end

@interface SBHive
@property __attribute__((iboutletcollection(B))) SBElementArray *bees;
@end
