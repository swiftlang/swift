@interface NSObject
+ alloc;
- init;
+ new;
@end

@interface A : NSObject
- (int)method:(int)arg withDouble:(double)d;
+ (int)classMethod;
+ (int)classMethod:(int)arg;
- (int)counter;
@property int counter;
- (void)setCounter:(int)value;

- (int)informalProp;

- (int)informalMadeFormal;

@property int overriddenProp;
@end

@interface B : A
- (int)method:(int)arg withFloat:(float)f;
+ (int)classMethod:(int)arg withInt:(int)i;

@property (readonly) int readCounter;

@property int informalMadeFormal;

@property int overriddenProp;

- init;
- initWithInt:(int)i;
- initWithInt:(int)i andDouble:(double)d;
@end

@interface A(Cat1)
- method:(int)i onCat1:(double)d;
@end

@interface A()
- method:(int)i onExtA:(double)d;
@end

@interface B()
- method:(int)i onExtB:(double)d;
+ newWithA:(A*)a;
@end

@interface A(Subscripting)
- objectAtIndexedSubscript:(unsigned)idx;
- (void)setObject:(id)object atIndexedSubscript:(unsigned)idx;

- objectForKeyedSubscript:(id)key;
- (void)setObject:(id)object forKeyedSubscript:(id)key;
@end

@interface NSDate : NSObject
- (signed char)isEqualToDate:(NSDate *)anotherDate;
@end

NSDate *swift_createDate(void);

@interface NSProxy
+ alloc;
@end

@interface AProxy : NSProxy
- initWithInt:(int)i;
@end

