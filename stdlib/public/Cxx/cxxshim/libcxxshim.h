template <class From, class To>
To _Nonnull __swift_interopStaticCast(From _Nonnull from) {
  return static_cast<To>(from);
}

namespace __swift_interop {
extern "C" {
void swift_retain(void *_Nonnull) noexcept;
void swift_release(void *_Nonnull) noexcept;
}
} // namespace __swift_interop

struct __swift_interop_closure {
  void *_Nonnull func;
  void *_Nullable context;

  inline void retain() const {
    if (context)
      __swift_interop::swift_retain(context);
  }

  inline void release() const {
    if (context)
      __swift_interop::swift_release(context);
  }
};
