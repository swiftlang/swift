struct HasThreeFields {
  int a = 1;
  int b = 2;
  int c = 3;
};

struct DerivedWithSameField : HasThreeFields {
  int a = 2;
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

struct OneField {
  int value = 42;
};

struct DerivedFromOneField : OneField {};

// Non trivial types:

struct __attribute__((swift_attr("import_unsafe"))) NonTrivial {
  NonTrivial() {}
  NonTrivial(const NonTrivial &) {}
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
  NonTrivialHasOneField(const NonTrivialHasOneField &other) : e(other.e) {}
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

int &getCopyCounter() {
    static int copyCounter = 0;
    return copyCounter;
}

class CopyTrackedBaseClass {
public:
    CopyTrackedBaseClass(int x) : x(x) {}
    CopyTrackedBaseClass(const CopyTrackedBaseClass &other) : x(other.x) {
        ++getCopyCounter();
    }

    int x;
};

class CopyTrackedDerivedClass: public CopyTrackedBaseClass {
public:
    CopyTrackedDerivedClass(int x) : CopyTrackedBaseClass(x) {}
};

class NonEmptyBase {
public:
    int getY() const {
        return y;
    }
private:
    int y = 11;
};

class CopyTrackedDerivedDerivedClass: public NonEmptyBase, public CopyTrackedDerivedClass {
public:
    CopyTrackedDerivedDerivedClass(int x) : CopyTrackedDerivedClass(x) {}
};

// Types with virtual methods, make sure field offsets are right. rdar://126754931

struct HasOneFieldWithVirtualMethod {
  int a;
  virtual ~HasOneFieldWithVirtualMethod() {}
};

struct HasTwoFieldsWithVirtualMethod {
  bool b;
  bool c;
  virtual ~HasTwoFieldsWithVirtualMethod() = default;
};

struct InheritFromStructsWithVirtualMethod: HasOneFieldWithVirtualMethod, HasTwoFieldsWithVirtualMethod {
  int d;
  virtual ~InheritFromStructsWithVirtualMethod() = default;
};

// MARK: Types that pack their fields into tail padding of a base class.

struct BaseAlign8 {
  long long field8 = 123;
}; // sizeof=8, dsize=8, align=8

struct DerivedHasTailPadding : public BaseAlign8 {
  int field4 = 456;
}; // sizeof=16, dsize=12, align=8

struct DerivedUsesBaseTailPadding : public DerivedHasTailPadding {
  short field2 = 789;
}; // sizeof=16, dsize=14, align=8

// MARK: Types with an out-of-order inheritance.

struct BaseWithVirtualDestructor {
  int baseField = 123;

  virtual ~BaseWithVirtualDestructor() {}
};

struct DerivedWithVirtualDestructor : public BaseWithVirtualDestructor {
  int derivedField = 456;

  ~DerivedWithVirtualDestructor() override {}
};

struct DerivedOutOfOrder : public HasOneField,
                           public DerivedWithVirtualDestructor {
  // DerivedWithVirtualDestructor is the primary base class despite being the
  // second one the list.

  int leafField = 789;

  ~DerivedOutOfOrder() override {}
};
