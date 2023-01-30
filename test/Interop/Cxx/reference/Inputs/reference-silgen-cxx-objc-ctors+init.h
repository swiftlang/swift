

@interface NSObject
@end

struct IntWrapper {
  int value;
  IntWrapper() = delete;
  IntWrapper(const int &value): value(value) {};
};

@interface ObjCSwiftBridge : NSObject
- (instancetype)init  __attribute__((unavailable));
- (instancetype)initWithEmbedded:(const IntWrapper &)embedded __attribute__((objc_designated_initializer));
@end
