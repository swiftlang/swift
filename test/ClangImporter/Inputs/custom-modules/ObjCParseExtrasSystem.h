@import Foundation;

@interface NSUIntegerTests : NSObject
- (void)consumeUnsigned:(NSUInteger)arg;
- (void)consumeUnsigned:(unsigned)arg withTheNSUIntegerHere:(NSUInteger)here;
- (void)consumeUnsigned:(NSUInteger)arg andAnother:(NSUInteger)another;

- (NSUInteger)unsignedProducer;
- (NSUInteger)unsignedProducer:(NSUInteger)arg fromCount:(NSUInteger)count;
- (NSUInteger)normalProducer:(NSUInteger)arg fromUnsigned:(NSUInteger)count;

@property NSUInteger normalProp;
@property NSUInteger unsignedProp;
@end

NSUInteger testUnsigned(NSUInteger a, NSUInteger b);
