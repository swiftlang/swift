#import <Foundation/Foundation.h>


// NB: No NS_ASSUME_NONNULL

@protocol FooProtocol <NSObject>

@property (nonatomic,readonly) NSString *name;

@end


@interface ParentClass : NSObject

- (instancetype)initWithFoo:(id<FooProtocol>)foo;

@end


