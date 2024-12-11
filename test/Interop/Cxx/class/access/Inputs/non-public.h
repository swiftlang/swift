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
  enum publEnumClosed { publEnumClosedValue1 } __attribute__((enum_extensibility(closed)));
  enum publEnumOpen { publEnumOpenValue1 } __attribute__((enum_extensibility(open)));
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
  enum privEnumClosed { privEnumClosedValue1 } __attribute__((enum_extensibility(closed)));
  enum privEnumOpen { privEnumOpenValue1 } __attribute__((enum_extensibility(open)));
  enum privEnumFlag {} __attribute__((flag_enum));
};

#endif /* NON_PUBLIC_H */
