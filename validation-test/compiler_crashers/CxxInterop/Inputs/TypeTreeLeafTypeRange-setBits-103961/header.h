struct Base {
  int b;
};

struct __attribute__((swift_attr("~Copyable"))) NonCopyableDerived : Base {
  int first;
  int second;
};
