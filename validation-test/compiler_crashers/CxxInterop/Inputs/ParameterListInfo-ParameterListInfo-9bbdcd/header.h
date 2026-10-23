struct S {
  ~S();
};

template <typename T>
void printT(const S &x, T t) __attribute__((swift_name("S.printT(self:t:)")));
