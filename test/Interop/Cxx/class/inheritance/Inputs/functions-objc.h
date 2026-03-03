#import <Foundation/Foundation.h>

@interface C : NSObject
@end

struct Base {
  C *_Nonnull non_virtual_method() const;
  virtual C *_Nonnull virtual_method() const;
  int a;
};

struct Derived : Base {};
