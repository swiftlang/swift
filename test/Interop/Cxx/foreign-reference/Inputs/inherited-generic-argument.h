#define IMMORTAL_FRT                                                           \
  __attribute__((swift_attr("import_reference")))                              \
  __attribute__((swift_attr("retain:immortal")))                              \
  __attribute__((swift_attr("release:immortal")))

struct IMMORTAL_FRT BaseObj {
  virtual int tag() const { return 0; }
};

struct DerivedObj1 : BaseObj {
  int tag() const override { return 1; }
  static DerivedObj1 &make() {
    static DerivedObj1 instance;
    return instance;
  }
};

struct DerivedObj2 : BaseObj {
  int tag() const override { return 2; }
  static DerivedObj2 &make() {
    static DerivedObj2 instance;
    return instance;
  }
};
