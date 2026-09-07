#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_STATIC_FACTORY_AS_INIT_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_STATIC_FACTORY_AS_INIT_H

// A static member function annotated with SWIFT_NAME("init(...)") is imported
// as a Swift initializer even though it is not a C++ constructor.

/// The static factory is an ordinary (non-template) member function.
struct NonTemplateFactory {
  int value;

  __attribute__((swift_name("init(fromInt:)"))) static NonTemplateFactory
  make(int v) {
    return NonTemplateFactory{v};
  }
};

/// The static factory is a member function template whose only template
/// parameter is defaulted and does not appear in the signature.
///
/// Imported as a non-generic initializer.
struct DefaultedTemplateParamFactory {
  int value;

  template <class T = int>
  __attribute__((swift_name("init(fromDefaultedTemplate:)")))
  static DefaultedTemplateParamFactory
  make(int v) {
    return DefaultedTemplateParamFactory{v};
  }
};

/// Overloaded static factories, mixing templated and non-templated overloads.
struct OverloadedFactories {
  int value;

  __attribute__((swift_name("init(overload:)")))
  static OverloadedFactories
  make(int v) {
    return OverloadedFactories{v};
  }

  template <class T = int>
  __attribute__((swift_name("init(overload:extra:)")))
  static OverloadedFactories
  make(int v, int w) {
    return OverloadedFactories{v + w};
  }
};

/// A static factory named 'init(...)' alongside a real C++ constructor, which
/// it must not interfere with.
struct FactoryAndConstructor {
  int value;

  FactoryAndConstructor(int v) : value(v) {}

  __attribute__((swift_name("init(fromFactory:)")))
  static FactoryAndConstructor
  make(int v) {
    return FactoryAndConstructor{v};
  }
};

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_STATIC_FACTORY_AS_INIT_H
