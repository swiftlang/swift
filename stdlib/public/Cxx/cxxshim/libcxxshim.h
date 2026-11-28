template <class T, class = void>
struct __swift_subclassShimAllocator {
  static void *allocate() { return __builtin_operator_new(sizeof(T)); }
};

template <class T>
struct __swift_subclassShimAllocator<
    T, decltype((void)T::operator new(sizeof(T)))> {
  static void *allocate() { return T::operator new(sizeof(T)); }
};

/// Backing storage for a Swift class that subclasses a C++ foreign reference
/// type `Base` and adds stored properties.
template <class Base, unsigned long Size, unsigned long Align>
struct __SwiftSubclassShim : Base {
  alignas(Align) char __swiftFields[Size];

  // Base must have a virtual destructor.
  ~__SwiftSubclassShim() override {}

  static __SwiftSubclassShim *__swift_allocate() {
#ifdef __STDCPP_DEFAULT_NEW_ALIGNMENT__
    static_assert(alignof(__SwiftSubclassShim) <=
                      __STDCPP_DEFAULT_NEW_ALIGNMENT__,
                  "over-aligned subclass is not supported");
#endif
    return static_cast<__SwiftSubclassShim *>(
        __swift_subclassShimAllocator<__SwiftSubclassShim>::allocate());
  }
};
