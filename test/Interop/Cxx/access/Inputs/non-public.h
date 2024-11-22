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

#define members(prefix)                                                        \
  void prefix##Method(void) const {}                                           \
  void prefix##MutatingMethod(void) {}                                         \
  int prefix##Var;                                                             \
  static void prefix##StaticFunc(void);                                        \
  static int prefix##StaticVar;                                                \
  enum prefix##Enum{prefix##EnumCase};                                         \
  typedef int prefix##Typedef;                                                 \
  struct prefix##Struct {}

TEST_CLASS __attribute__((
    __swift_attr__("private_fileid:main/blessed.swift"))) MyClass {
public:
  members(pub);
TEST_PRIVATE:
  members(priv);
};

#endif /* NON_PUBLIC_H */
