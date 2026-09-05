#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_STATIC_FACTORY_AS_INIT_GENERIC_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_STATIC_FACTORY_AS_INIT_GENERIC_H

// Static member function templates renamed to 'init(...)' with SWIFT_NAME,
// where the template parameter appears in the signature so the initializer has
// to be imported as a generic initializer.

/// One template parameter, used as the only parameter.
struct GenericFactory {
  int value;

  template <class T>
  __attribute__((swift_name("init(fromGeneric:)"))) static GenericFactory
  make(T v) {
    return GenericFactory{static_cast<int>(v)};
  }
};

/// Two template parameters, both used.
struct MultiGenericFactory {
  int value;

  template <class T, class U>
  __attribute__((swift_name("init(_:other:)"))) static MultiGenericFactory
  make(T v, U w) {
    return MultiGenericFactory{static_cast<int>(v) + static_cast<int>(w)};
  }
};

/// A template parameter used alongside a concrete parameter.
struct MixedGenericFactory {
  int value;

  template <class T>
  __attribute__((swift_name("init(generic:concrete:)"))) static
      MixedGenericFactory
      make(T v, int w) {
    return MixedGenericFactory{static_cast<int>(v) + w};
  }
};

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_STATIC_FACTORY_AS_INIT_GENERIC_H
