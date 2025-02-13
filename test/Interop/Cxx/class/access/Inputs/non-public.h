#ifndef NON_PUBLIC_H
#define NON_PUBLIC_H

// Override this to test structs
#ifndef TEST_CLASS
#define TEST_CLASS class
#endif

// Override this to test protected
#ifndef TEST_PRIVATE
#define TEST_PRIVATE private
#endif

/// A C++ class with various kinds of public and non-public fields, all of which
/// should be imported. Non-public fields should only be accessible inside
/// MyClass extensions in blessed.swift.
TEST_CLASS
__attribute__((__swift_attr__("private_fileid:main/blessed.swift"))) MyClass {

public:
  void publMethod(void) const {}
  void publMutatingMethod(void) {}
  int publVar;
  static void publStaticFunc(void);
  static int publStaticVar;

  typedef int publTypedef;
  struct publStruct {};

  enum publEnum { publEnumValue1 };
  enum class publEnumClass { publEnumClassValue1 };
  enum { publEnumAnonValue1 };
  enum publEnumClosed {
    publEnumClosedValue1
  } __attribute__((enum_extensibility(closed)));
  enum publEnumOpen {
    publEnumOpenValue1
  } __attribute__((enum_extensibility(open)));
  enum publEnumFlag {} __attribute__((flag_enum));

TEST_PRIVATE:
  void privMethod(void) const {}
  void privMutatingMethod(void) {}
  int privVar;
  static void privStaticFunc(void);
  static int privStaticVar;

  typedef int privTypedef;
  struct privStruct {};

  enum privEnum { privEnumValue1 };
  enum class privEnumClass { privEnumClassValue1 };
  enum { privEnumAnonValue1 };
  enum privEnumClosed {
    privEnumClosedValue1
  } __attribute__((enum_extensibility(closed)));
  enum privEnumOpen {
    privEnumOpenValue1
  } __attribute__((enum_extensibility(open)));
  enum privEnumFlag {} __attribute__((flag_enum));
};

/// A C++ templated class, whose non-public fields should be accessible in
/// extensions of the (instantiated) class in blessed.swift.
template <typename T>
TEST_CLASS __attribute__((
    __swift_attr__("private_fileid:main/blessed.swift"))) MyClassTemplate {
public:
  T publMethodT(T t) const { return t; }
  T publVarT;
  typedef T publTypedefT;

  void publMethod(void) const {}
  int publVar;
  typedef int publTypedef;
TEST_PRIVATE:
  T privMethodT(T t) const { return t; }
  T privVarT;
  typedef T privTypedefT;

  void privMethod(void) const {}
  int privVar;
  typedef int privTypedef;
};

typedef MyClassTemplate<float> MyFloatyClass;
typedef MyClassTemplate<MyClass> MyClassyClass;

#endif /* NON_PUBLIC_H */
