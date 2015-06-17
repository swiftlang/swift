#ifndef OBJC_NSOBJECT_H_
#define OBJC_NSOBJECT_H_

#include <objc/objc.h>

@class NSString;

@protocol NSObject
@property (readonly, copy) NSString *description;
- (instancetype)retain OBJC_ARC_UNAVAILABLE;
- (Class)class;
- (BOOL)conformsToProtocol:(Protocol *)aProtocol;
@end

@interface NSObject <NSObject>
+ alloc;
- init;
- (void)dealloc;
+ new;
- performSelector:(SEL)selector withObject:(id)obj;
- (Class)myClass;
+ (Class)class;
+ description;
- (BOOL)allowsWeakReference __attribute__((unavailable));
- (BOOL)isEqual:(NSObject *)other;
@property (readonly) NSInteger hash;
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
- init;

@property int overriddenProp;
@end

@protocol BProto
- (int)method:(int)arg withFloat:(float)f;
- (int)otherMethod:(int)arg withFloat:(float)f;
@end

@protocol Cat1Proto
- cat1Method;
@end

@interface B : A <BProto>
- (int)method:(int)arg withFloat:(float)f;
+ (int)classMethod:(int)arg withInt:(int)i;
- (id<BProto>)getAsProto;
- (id<BProto, Cat1Proto>)getAsProtoWithCat;
- performAdd:(int)x withValue:(int)y withValue:(int)z withValue2:(int)w;
- performMultiplyWithValue:(int)x value:(int)y;
- moveFor:(int)x;
@property (readonly) int readCounter;

@property int informalMadeFormal;

@property int overriddenProp;

@property Protocol *protocol;
- (void)doThing:(id)thing protocol:(Protocol *)protocol;
- (BOOL)protocol:(Protocol *)protocol hasThing:(id)thing;

- initWithInt:(int)i;
- initWithInt:(int)i andDouble:(double)d;
- initWithDouble:(double)d1 :(double)d2;
- initBBB:(B*)b;
- initForWorldDomination;
- notAnInit __attribute__((objc_method_family(init), ns_returns_retained));
- (id)_initFoo;
- (void)anotherMethodOnB;

+ (void)instanceTakesObjectClassTakesFloat:(float)x;
- (void)instanceTakesObjectClassTakesFloat:(id)x;

@end

@interface A(Cat1) <Cat1Proto>
- method:(int)i onCat1:(double)d;
- cat1Method;
@end

@interface A()
- method:(int)i onExtA:(double)d;
@end

@interface B()
- method:(int)i onExtB:(double)d;
+ newWithA:(A*)a;
@end

@interface A(Subscripting)
- objectAtIndexedSubscript:(NSInteger)idx;
- (void)setObject:(id)object atIndexedSubscript:(NSInteger)idx;

- objectForKeyedSubscript:(id)key;
@end

@interface B(Subscripting)
- (void)setObject:(id)object forKeyedSubscript:(id)key;
@end

@protocol P2
- (void)p2Method;
- (id)initViaP2:(double)x second:(double)y;
@end

@interface B(P2) <P2>
@end

@interface NSDate : NSObject
- (signed char)isEqualToDate:(NSDate *)anotherDate;
@end

@interface NSProxy
+ alloc;
@end

@interface AProxy : NSProxy
- initWithInt:(int)i;
@end

@interface A(BoolStuff)
- setEnabled:(BOOL)enabled;
@end

@interface AlmostSubscriptable
- (A*) objectForKeyedSubscript:(id)key;
- (void)setObject:(id)object forKeyedSubscript:(id)key;
@end

#endif
