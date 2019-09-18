
#if __has_include(<Foundation/Foundation.h>)

#import "NSSlowString.h"


@interface NSSlowString ()

@property (nonatomic, strong) NSString *stringHolder;

@end

@implementation NSSlowString

- (instancetype)initWithString:(NSString *)name {
	self = [super init];
	if (self == nil) {
		return nil;
	}
	self.stringHolder = name;
	return self;
}

- (instancetype)initWithCharacters:(const unichar * _Nonnull)chars length:(NSUInteger)count {
  NSString *str = [[NSString alloc] initWithCharacters: chars length: count];
  self = [self initWithString: str];
  return self;
}

- (NSUInteger)length {
    return self.stringHolder.length;
}

- (id)copyWithZone:(NSZone *)unused {
	return self;
}

- (unichar)characterAtIndex:(NSUInteger)index {
    return [self.stringHolder characterAtIndex:index];
}

- (void *) _fastCharacterContents {
  return nil;
}

@end

#endif

