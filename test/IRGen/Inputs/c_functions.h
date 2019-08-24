// This header is included on non-ObjC platforms.

void overloaded(void) __attribute__((overloadable));
void overloaded(int) __attribute__((overloadable));

extern void use(const char *);

static inline void test_my_log() {
  __attribute__((internal_linkage)) static const char fmt[] = "foobar";
  use(fmt);
}

extern void useInt(unsigned int);

typedef struct {
    unsigned int val[8];
} a_thing;

static inline void log_a_thing(const a_thing thing) {
 useInt(thing.val[0]);
 useInt(thing.val[7]);
}

static inline unsigned int return7(void) {
  return 7;
}
