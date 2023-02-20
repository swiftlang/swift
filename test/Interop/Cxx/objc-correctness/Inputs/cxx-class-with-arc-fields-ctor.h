#import <Foundation/Foundation.h>

struct S {
  NSString *_Nullable A;
  NSString *_Nullable B;
  NSString *_Nullable C;

  void dump() const {
    printf("%s\n", [A UTF8String]);
    printf("%s\n", [B UTF8String]);
    printf("%s\n", [C UTF8String]);
  }
};

