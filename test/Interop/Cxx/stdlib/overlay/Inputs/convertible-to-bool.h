struct BoolBox {
  bool value;

  operator bool() const { return value; }
};

struct NonConstBoolBox {
  bool value;

  operator bool() { return value; }
};

struct DualOverloadBoolBox {
  bool value;

  operator bool() const { return value; }
  operator bool() { return value; }
};

struct ExplicitBoolBox {
private:
  bool value;

public:
  explicit operator bool() const { return value; }
};
