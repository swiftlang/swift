@import Foundation;

@interface Foo: NSObject

- (BOOL)foo:(int)x error:(NSError**)error;
- (BOOL)foothrows:(int)x error:(NSError**)error;

@end
