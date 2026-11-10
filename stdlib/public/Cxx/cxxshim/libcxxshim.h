/// Backing storage for a Swift class that subclasses a C++ foreign reference
/// type `Base` and adds stored properties.
template <class Base, unsigned long Size, unsigned long Align>
struct __SwiftSubclassShim : Base {
  alignas(Align) char __swiftFields[Size];

  // Base must have a virtual destructor.
  ~__SwiftSubclassShim() override {}

  static __SwiftSubclassShim *__swift_allocate() {
    return static_cast<__SwiftSubclassShim *>(
        __builtin_operator_new(sizeof(__SwiftSubclassShim)));
  }
};
