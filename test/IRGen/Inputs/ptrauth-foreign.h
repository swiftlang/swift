struct IntPair {
  int x;
  int y;
};

__attribute__((objc_root_class))
@interface Root
@end

@interface ObjCWidget : Root
@end

typedef struct __attribute__((objc_bridge(ObjCWidget))) __widget *WidgetRef;