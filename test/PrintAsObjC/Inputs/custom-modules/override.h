@import Foundation;

@interface Base : NSObject
- (NSUInteger)foo;
- (NSUInteger)foo:(NSUInteger)ignored;
- (NSUInteger)foo:(NSUInteger)x y:(NSUInteger)y;

@property(readonly,getter=getProp) NSUInteger prop;

- (id)objectAtIndexedSubscript:(NSUInteger)idx;

- (void)callback:(NSUInteger(^)(void))block;
@end
