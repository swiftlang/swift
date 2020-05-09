#import <Foundation/Foundation.h>

extern NSString * _Nonnull (^ _Nonnull mutableBlockGlobal)(NSString * _Nonnull);
extern NSString * _Nonnull (^ _Nonnull const constBlockGlobal)(NSString * _Nonnull);

extern NSString * _Nonnull (* _Nonnull mutableFPGlobal)(NSString * _Nonnull);
extern NSString * _Nonnull (* _Nonnull const constFPGlobal)(NSString * _Nonnull);
