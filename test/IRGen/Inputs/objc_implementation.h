#import <Foundation/Foundation.h>

@interface ImplClass: NSObject

- (nonnull instancetype)init;

@property (assign) int implProperty;

- (void)mainMethod:(int)param;

@end


@interface ImplClass (Category1)

- (void)category1Method:(int)param;

@end


@interface ImplClass (Category2)

- (void)category2Method:(int)param;

@end


@interface NoImplClass

- (void)noImplMethod:(int)param;

@end
