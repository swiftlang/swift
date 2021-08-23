@class C;
@interface C
{}
@end

struct S {
  union {
    C *t;
    char c;
  };
  S(const S &s) {}
  ~S() { }
  int f() { return 42; }
};

S *getSPtr();
