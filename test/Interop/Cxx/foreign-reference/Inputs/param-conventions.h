#ifndef TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_PARAM_CONVENTIONS_H
#define TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_PARAM_CONVENTIONS_H

struct Shared;
void retainShared(Shared *_Nonnull);
void releaseShared(Shared *_Nonnull);

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainShared")))
__attribute__((swift_attr("release:releaseShared"))) Shared {
  int value() const;
  virtual int virtualValue() const;
  // An rvalue `this` is imported as `consuming`, but C++ does not release it.
  void onRValue() &&;
};

// A C or C++ function takes a pointer or reference argument at +1 only if the
// parameter is consumed.
void takesPtr(Shared *_Nonnull s);
void takesNullablePtr(Shared *_Nullable s);
void takesRef(Shared &s);
void takesConstRef(const Shared &s);
void consumesPtr(Shared *_Nonnull __attribute__((cf_consumed)) s);
void consumesNullablePtr(Shared *_Nullable __attribute__((cf_consumed)) s);
int callWith(int (*_Nonnull fn)(Shared *_Nonnull), Shared *_Nonnull s);

// An immortal foreign reference type is trivial, so it is never retained.
struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal"))) Immortal {
  int value() const;
};

void takesImmortal(Immortal *_Nonnull i);

// A call the optimizer cannot see through.
void opaque();

#endif // TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_PARAM_CONVENTIONS_H
