#import "objc_implicit_inner_pointer.h"

@implementation Foo {
  CFTypeRef _bar;
}

- (id)init {
  _bar = (__bridge_retained CFTypeRef)[@"1234567891" mutableCopy];
  return self;
}

- (CFTypeRef)bar {
  return _bar;
}

- (CFTypeRef)nullabar {
  return _bar;
}

- (void)dealloc {
  printf("%s", __FUNCTION__);

  if (_bar)
    CFRelease(_bar);
}

@end
