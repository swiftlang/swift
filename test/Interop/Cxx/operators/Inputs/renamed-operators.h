struct HasRenamedOperatorStar {
  int value;

  const int &operator*() const __attribute__((swift_name("dereference()"))) {
    return value;
  }
};

struct HasRenamedOperatorPlusPlus {
  int value;

  HasRenamedOperatorPlusPlus &operator++() __attribute__((swift_name("plusPlus()"))) {
    value++;
    return *this;
  }
};
