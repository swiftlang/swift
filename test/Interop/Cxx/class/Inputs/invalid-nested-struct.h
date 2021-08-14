// When we import this class, make sure that we bail before trying to import
// its sub-decls (i.e., "ForwardDeclaredSibling").
struct CannotImport {
  void test(struct ForwardDeclaredSibling *) {}

  ~CannotImport() = delete;
  CannotImport(CannotImport const &) = delete;
};

// We shouldn't be able to import this class either because it also doesn't have
// a copy ctor or destructor.
struct ForwardDeclaredSibling : CannotImport {};

// This is a longer regression test to make sure we don't improperly cache a
// typedef that's invalid.
namespace RegressionTest {

template <class From>
struct pointer_traits {
  template <class To>
  struct rebind {
    typedef To other;
  };
};

template <class T, class U>
struct Convert {
  typedef typename pointer_traits<T>::template rebind<U>::other type;
};

template <class>
struct Forward;

template <class V>
struct SomeTypeTrait {
  typedef Forward<V> *F;
  typedef typename Convert<V, F>::type A;
};

template <class V>
struct Forward {
  typedef typename SomeTypeTrait<V>::A A;

private:
  ~Forward() = delete;
  Forward(Forward const &) = delete;
};

template <class V>
struct SomeOtherTypeTrait : SomeTypeTrait<V> {
  typedef typename SomeTypeTrait<V>::A A;
};

// Just to instantiate all the templates.
struct FinalUser {
  typedef Forward<void *> F;
  typedef SomeOtherTypeTrait<void *> O;
  typedef SomeTypeTrait<void *> Z;
};

void test(typename FinalUser::Z) {}

} // namespace RegressionTest
