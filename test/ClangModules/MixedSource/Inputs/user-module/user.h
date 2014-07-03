@import Foundation;

#define testInMacro NSUInteger testFunctionInsideMacro(NSUInteger I);

@interface NSUIntTest : NSObject <NSFastEnumeration>
@property NSUInteger IntProp;
- (NSUInteger)myCustomMethodThatOperatesOnNSUIntegers: (NSUInteger)key;
- (NSUInteger)countByEnumeratingWithState:(NSFastEnumerationState *)state objects:(id *)stackbuf count:(NSUInteger)len;
@end

NSUInteger testFunction(NSUInteger input);

testInMacro

NSUInteger testFunctionWithPointerParam(NSUInteger *input);
