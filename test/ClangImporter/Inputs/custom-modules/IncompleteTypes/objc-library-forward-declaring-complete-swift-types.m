#import "objc-library-forward-declaring-complete-swift-types.h"
#import "CompleteSwiftTypes-Swift.h"

void takeAFoo(Foo *foo) { [foo sayHello]; }

Foo *returnAFoo() {
  Foo *result = [[Foo alloc] init];
  [result sayHello];
  return result;
}

void takeABaz(Baz *baz) { [baz sayHello]; }

Baz *returnABaz() {
  Baz *result = [[Baz alloc] init];
  [result sayHello];
  return result;
}
