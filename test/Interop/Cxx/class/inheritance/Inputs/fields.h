struct HasThreeFields {
  int a = 1;
  int b = 2;
  int c = 3;
};

struct DerivedWithOneField : HasThreeFields {
  int d = 4;
};

struct HasOneField {
  int e = 5;
};

struct DerivedFromAll : HasOneField, DerivedWithOneField {
  int f = 6;
};

// Non trivial types:

struct __attribute__((swift_attr("import_unsafe"))) NonTrivial {
  NonTrivial() {}
  ~NonTrivial() {}
};

struct NonTrivialHasThreeFields : NonTrivial {
  int a = 1;
  int b = 2;
  int c = 3;
};

struct NonTrivialDerivedWithOneField : NonTrivialHasThreeFields {
  int d = 4;
};

struct __attribute__((swift_attr("import_unsafe"))) NonTrivialHasOneField {
  NonTrivialHasOneField() {}
  ~NonTrivialHasOneField() {}

  int e = 5;
};

struct __attribute__((swift_attr("import_unsafe"))) NonTrivialDerivedFromAll
    : NonTrivialHasOneField,
      NonTrivialDerivedWithOneField {
  int f = 6;
};

// Templates:

template<class T>
struct ClassTemplate {
  T value;
};

struct DerivedFromClassTemplate : ClassTemplate<int> {};