void doSomethingInHead(int arg);

@interface BaseInHead
- (void)doIt:(int)arg;
@end

static inline void test1(int x) {
  if (x = 0) {} // produce a warning.
}
