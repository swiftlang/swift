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

- (NSUInteger)length {
    return self.stringHolder.length;
}

- (id)copy {
	return self;
}

- (unichar)characterAtIndex:(NSUInteger)index {
    return [self.stringHolder characterAtIndex:index];
}

- (void *) _fastCharacterContents {
  return nil;
}

@end