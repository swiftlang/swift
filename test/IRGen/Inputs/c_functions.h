// This header is included on non-ObjC platforms.

void overloaded(void) __attribute__((overloadable));
void overloaded(int) __attribute__((overloadable));

extern void use(const char *);

static inline void test_my_log() {
  __attribute__((internal_linkage)) static const char fmt[] = "foobar";
  use(fmt);
}
