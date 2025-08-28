template <class T>
struct __attribute__((swift_attr("~Copyable"))) HasUninstantiatableCopyConstructor {
  template <class U>
  void scaryPoison(U u) {
    U::doesNotExist(u);
  }

  int value;
  HasUninstantiatableCopyConstructor(int value) : value(value) {}
  HasUninstantiatableCopyConstructor(
      const HasUninstantiatableCopyConstructor &other) {
    scaryPoison(other);
  }
  HasUninstantiatableCopyConstructor(
      HasUninstantiatableCopyConstructor &&other) = default;
};

typedef HasUninstantiatableCopyConstructor<int> NonCopyableInst;

template <class T>
struct __attribute__((swift_attr("~Copyable"))) DerivedUninstantiatableCopyConstructor : HasUninstantiatableCopyConstructor<T> {
  DerivedUninstantiatableCopyConstructor(int value) : HasUninstantiatableCopyConstructor<T>(value) {}
  DerivedUninstantiatableCopyConstructor(const DerivedUninstantiatableCopyConstructor &other) = default;
  DerivedUninstantiatableCopyConstructor(DerivedUninstantiatableCopyConstructor &&other) = default;
};

typedef DerivedUninstantiatableCopyConstructor<int> DerivedNonCopyableInst;
