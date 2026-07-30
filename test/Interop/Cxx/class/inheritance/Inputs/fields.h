#if defined(_MSC_VER)
#define NO_UNIQUE_ADDRESS [[msvc::no_unique_address]]
#else
#define NO_UNIQUE_ADDRESS [[no_unique_address]]
#endif

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

struct Empty {};

struct BaseEmpty {
  Empty e;
};

inline BaseEmpty makeBaseEmpty() { return {}; }

struct EmptyOnlyDerived : BaseEmpty {};

inline EmptyOnlyDerived makeEmptyOnlyDerived() { return {}; }

struct BaseEmptyAndInt : BaseEmpty {
  int x;
};

inline BaseEmptyAndInt makeBaseEmptyAndInt(int n) { return {{}, n}; }

struct IntAndEmpty {
  int x;
  Empty e;
};

inline IntAndEmpty makeIntAndEmpty() {
  IntAndEmpty v;
  v.x = 42;
  return v;
}

struct NoUniqueAddressEmpty {
  NO_UNIQUE_ADDRESS Empty e;
  int x;
};

inline NoUniqueAddressEmpty makeNoUniqueAddressEmpty() {
  NoUniqueAddressEmpty v;
  v.x = 17;
  return v;
}

struct EmptyBaseA {};
struct EmptyBaseB {};
struct MultiEmptyBases : EmptyBaseA, EmptyBaseB {};

inline MultiEmptyBases makeMultiEmptyBases() { return {}; }

struct BaseWithEmpty1 {
  Empty e1;
};
struct BaseWithEmpty2 {
  Empty e2;
};
struct MultiBaseWithEmpty : BaseWithEmpty1, BaseWithEmpty2 {};

inline MultiBaseWithEmpty makeMultiBaseWithEmpty() { return {}; }

class ContainsEmptyClass {
public:
  Empty e;
};

inline ContainsEmptyClass makeContainsEmptyClass() { return {}; }

struct OuterWithEmpty {
  struct Inner {};
  Inner i;
  int x;
};

inline OuterWithEmpty makeOuterWithEmpty() {
  OuterWithEmpty v;
  v.x = 7;
  return v;
}

struct EmptyArrayHolder {
  Empty arr[3];
};

inline EmptyArrayHolder makeEmptyArrayHolder() { return {}; }

template <class T>
struct EmptyHolder {
  T t;
  bool x;
};

inline EmptyHolder<Empty> makeEmptyHolderOfEmpty() {
  EmptyHolder<Empty> v;
  v.x = false;
  return v;
}

inline EmptyHolder<int> makeEmptyHolderOfInt() {
  EmptyHolder<int> v;
  v.t = 2;
  v.x = false;
  return v;
}

// Has a vfptr so it's not empty even though it has no data members.
struct VirtualDtor {
  virtual ~VirtualDtor() {}
};
struct HasFieldWithOnlyVDtor {
  VirtualDtor v;
};

inline HasFieldWithOnlyVDtor makeHasFieldWithOnlyVDtor() { return {}; }

struct EmptyForVBase {};
struct DerivedWithVBase : virtual EmptyForVBase {};

struct WrapsVirtualBase {
  DerivedWithVBase d;
};

inline WrapsVirtualBase makeWrapsVirtualBase() { return {}; }

struct BaseEmptyAndTwoChars {
  Empty e;
  char a;
  char b;
};

struct ChildEmptyAndTwoChars : BaseEmptyAndTwoChars {};

inline ChildEmptyAndTwoChars makeChildEmptyAndTwoChars() {
  ChildEmptyAndTwoChars v;
  v.a = 'A';
  v.b = 'B';
  return v;
}

struct BaseEmptyIntChar {
  Empty e;
  int x;
  char y;
};

struct ChildEmptyIntChar : BaseEmptyIntChar {};

inline ChildEmptyIntChar makeChildEmptyIntChar() {
  ChildEmptyIntChar v;
  v.x = 42;
  v.y = 'Y';
  return v;
}

struct S1 {};

struct S2 {
  S1 member;
};

struct S3 {
  S2 field;
};

inline S3 makeS3() { return S3(); }

inline int takeBaseEmpty(BaseEmpty x, int n) { return n; }

inline BaseEmpty roundTripBaseEmpty(BaseEmpty x) { return x; }

inline EmptyOnlyDerived roundTripEmptyOnlyDerived(EmptyOnlyDerived x) {
  return x;
}

union UnionAllEmpty {
  Empty e1;
  Empty e2;
};

struct HasEmptyUnion {
  UnionAllEmpty u;
  int i;
};

inline HasEmptyUnion makeHasEmptyUnion() {
  HasEmptyUnion v;
  v.i = 3;
  return v;
}

union UnionEmptyAndInt {
  Empty e;
  int x;
};

struct HasUnionEmptyAndInt {
  UnionEmptyAndInt u;
  int i;
};

inline HasUnionEmptyAndInt makeHasUnionEmptyAndInt() {
  HasUnionEmptyAndInt v;
  v.i = 5;
  return v;
}

inline HasUnionEmptyAndInt makeHasUnionEmptyAndIntWithValue() {
  HasUnionEmptyAndInt v;
  v.i = 5;
  v.u.x = 15;
  return v;
}
