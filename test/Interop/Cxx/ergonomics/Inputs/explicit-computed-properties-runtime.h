#define SWIFT_COMPUTED_PROPERTY \
  __attribute__((swift_attr("import_computed_property")))

struct GetterSetter {
  int val = 42;
  int get_x() const SWIFT_COMPUTED_PROPERTY { return val; }
  void set_x(int v) SWIFT_COMPUTED_PROPERTY { val = v; }
};

struct GetterOnly {
  int get_value() const SWIFT_COMPUTED_PROPERTY { return 42; }
};

struct SnakeCaseAcronym {
  int val = 42;
  int get_http_URL() const SWIFT_COMPUTED_PROPERTY { return val; }
  void set_http_URL(int p) SWIFT_COMPUTED_PROPERTY { val = p; }
};

struct SnakeCaseNoPrefix {
  int im_snake_case_swift_computed_property() const SWIFT_COMPUTED_PROPERTY {
    return 42;
  }
};
