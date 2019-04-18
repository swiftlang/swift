
typedef __INTPTR_TYPE__ intptr_t;

__attribute__((objc_root_class))
@interface Base
- (instancetype)init;
@end

@interface IncompleteDesignatedInitializers : Base
- (instancetype)initFirst:(intptr_t)x __attribute__((objc_designated_initializer));
- (instancetype)initSecond:(intptr_t)x __attribute__((objc_designated_initializer));
- (instancetype)initMissing:(intptr_t)x, ... __attribute__((objc_designated_initializer));
- (instancetype)initConveniently:(intptr_t)x;
@end
@interface IncompleteDesignatedInitializers (CategoryConvenience)
- (instancetype)initCategory:(intptr_t)x;
@end

@interface IncompleteConvenienceInitializers : Base
- (instancetype)initFirst:(intptr_t)x __attribute__((objc_designated_initializer));
- (instancetype)initSecond:(intptr_t)x __attribute__((objc_designated_initializer));
- (instancetype)initMissing:(intptr_t)x, ...;
- (instancetype)initConveniently:(intptr_t)x;
@end
@interface IncompleteConvenienceInitializers (CategoryConvenience)
- (instancetype)initCategory:(intptr_t)x;
@end

@interface IncompleteUnknownInitializers : Base
- (instancetype)initFirst:(intptr_t)x;
- (instancetype)initSecond:(intptr_t)x;
- (instancetype)initMissing:(intptr_t)x, ...;
- (instancetype)initConveniently:(intptr_t)x;
@end
@interface IncompleteUnknownInitializers (CategoryConvenience)
- (instancetype)initCategory:(intptr_t)x;
@end

@interface IncompleteDesignatedInitializersWithCategory : Base
- (instancetype)initFirst:(intptr_t)x __attribute__((objc_designated_initializer));
- (instancetype)initMissing:(intptr_t)x, ... __attribute__((objc_designated_initializer));
- (instancetype)initConveniently:(intptr_t)x;
@end
@interface IncompleteDesignatedInitializersWithCategory (/*class extension*/)
- (instancetype)initSecond:(intptr_t)x __attribute__((objc_designated_initializer));
- (instancetype)initCategory:(intptr_t)x;
@end

@interface DesignatedInitializerInAnotherModule : Base
- (instancetype)initFirst:(intptr_t)x __attribute__((objc_designated_initializer));
- (instancetype)initSecond:(intptr_t)x __attribute__((objc_designated_initializer));
- (instancetype)initMissing:(intptr_t)x, ... __attribute__((objc_designated_initializer));
- (instancetype)initConveniently:(intptr_t)x;
@end
@interface DesignatedInitializerInAnotherModule (CategoryConvenience)
- (instancetype)initCategory:(intptr_t)x;
@end
