#ifndef SWIFT_GNUSTEP_COMPAT_OBJC_NSOBJECT_H
#define SWIFT_GNUSTEP_COMPAT_OBJC_NSOBJECT_H

#include <objc/objc.h>

#if defined(__LP64__) || defined(_LP64)
typedef long NSInteger;
typedef unsigned long NSUInteger;
#else
typedef int NSInteger;
typedef unsigned int NSUInteger;
#endif

#ifdef __OBJC__
@class NSString;

@protocol NSObject
- (instancetype)self;
- (Class)class;
- (BOOL)conformsToProtocol:(Protocol *)aProtocol;
- (BOOL)isKindOfClass:(Class)aClass;
- (BOOL)respondsToSelector:(SEL)aSelector;
@property (readonly, copy) NSString *description;
@end

@interface NSObject <NSObject>
+ (instancetype)alloc;
+ (instancetype)new;
+ (Class)class;
- (instancetype)init;
- (void)dealloc;
- (instancetype)self;
- (Class)class;
- (Class)superclass;
- (BOOL)isEqual:(id)other;
- (BOOL)conformsToProtocol:(Protocol *)aProtocol;
- (BOOL)isKindOfClass:(Class)aClass;
- (BOOL)respondsToSelector:(SEL)aSelector;
- (id)performSelector:(SEL)selector;
- (id)performSelector:(SEL)selector withObject:(id)obj;
- (id)performSelector:(SEL)selector
           withObject:(id)obj1
           withObject:(id)obj2;
@property (readonly) NSUInteger hash;
@end
#endif

#endif
