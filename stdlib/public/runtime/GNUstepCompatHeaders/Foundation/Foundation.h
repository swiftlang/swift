#ifndef SWIFT_GNUSTEP_COMPAT_FOUNDATION_H
#define SWIFT_GNUSTEP_COMPAT_FOUNDATION_H

#include <objc/NSObject.h>

typedef struct _NSZone NSZone;

@class NSString;
@class NSDictionary<KeyType, ObjectType>;
@class NSArray<ObjectType>;
@class NSSet<ObjectType>;
@class NSProxy;
@class NSError;
@class NSCoder;

@protocol NSCopying
- (id)copyWithZone:(NSZone *)zone;
@end

@protocol NSMutableCopying
- (id)mutableCopyWithZone:(NSZone *)zone;
@end

@interface NSString : NSObject <NSCopying, NSMutableCopying>
@property (readonly, copy) NSString *decomposedStringWithCanonicalMapping;
@end

@interface NSDictionary<KeyType, ObjectType> : NSObject
@property (readonly) NSUInteger count;
- (ObjectType)objectForKey:(KeyType)key;
@end

@interface NSArray<ObjectType> : NSObject
@end

@interface NSSet<ObjectType> : NSObject
@end

@interface NSNull : NSObject
+ (NSNull *)null;
@end

@interface NSError : NSObject
@property (readonly) NSString *domain;
@property (readonly) NSInteger code;
- (instancetype)initWithDomain:(NSString *)domain
                          code:(NSInteger)code
                      userInfo:(NSDictionary *)userInfo;
@end

@interface NSProxy
+ (instancetype)alloc;
@end

#endif
