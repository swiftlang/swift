@import Foundation;

@interface Base : NSObject
- (NSUInteger)foo;
- (NSUInteger)foo:(NSUInteger)ignored;
- (NSUInteger)foo:(NSUInteger)x y:(NSUInteger)y;

@property(readonly,getter=getProp) NSUInteger prop;
@property(readonly) NSInteger originalName __attribute__((swift_name("renamedProp")));

- (id)objectAtIndexedSubscript:(NSUInteger)idx;

- (void)callback:(NSUInteger(^)(void))block;

- (BOOL)doThingAndReturnError:(NSError **)error;
- (BOOL)doAnotherThingWithError:(NSError **)error;

@end

@protocol Proto <NSObject>

- (NSUInteger)proto;
- (NSUInteger)proto:(NSUInteger)ignored;
- (NSUInteger)proto:(NSUInteger)x y:(NSUInteger)y;

@end
