struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) IntBox {
  int value;
  IntBox(int value) : value(value) {}

  static IntBox *create(int value) { return new IntBox(value); }
};

inline int extractValueFromPtr(IntBox *b) { return b->value; }
inline int extractValueFromRef(IntBox &b) { return b.value; }
inline int extractValueFromConstRef(const IntBox &b) { return b.value; }
inline int extractValueFromRefToPtr(IntBox *&b) { return b->value; }
inline int extractValueFromRefToConstPtr(IntBox const *&b) { return b->value; }
inline int extractValueFromConstRefToPtr(IntBox *const &b) { return b->value; }
inline int extractValueFromConstRefToConstPtr(IntBox const *const &b) { return b->value; }
