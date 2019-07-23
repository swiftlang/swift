#import "Escaper.h"

#import "DangerousEscaper.h"

@interface Escaper () <DangerousEscaper>
  -(void)mightBeNaughty:(void (^)(void))completion;
@end

@implementation Escaper

- (id)init {
  if ((self = [super init]) != nil) {
    self.escape = ^{};
  }
  return self;
}

-(void)mightBeNaughty:(void (^)(void))completion {
  self.escape = completion;
  completion();
}

@end
