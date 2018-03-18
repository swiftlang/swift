// This file is meant to be used with the mock SDK, not the real one.
#import <Foundation.h>

typedef NSString *StructLikeStringWrapper __attribute__((swift_newtype(struct)));
typedef NSString *EnumLikeStringWrapper __attribute__((swift_newtype(enum)));

typedef NSObject *StructLikeObjectWrapper __attribute__((swift_newtype(struct)));
typedef NSError *StructLikeErrorWrapper __attribute__((swift_newtype(struct)));
