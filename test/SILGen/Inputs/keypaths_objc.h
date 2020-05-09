@import Foundation;

@interface ObjCFoo

@property(readonly) NSString *_Nonnull objcProp;

@end

union c_union {
  struct some_struct {
    void* data;
  } some_field;
};
