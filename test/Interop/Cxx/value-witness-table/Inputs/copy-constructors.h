#ifndef TEST_INTEROP_CXX_VALUE_WITNESS_TABLE_INPUTS_COPY_CONSTRUCTORS_H
#define TEST_INTEROP_CXX_VALUE_WITNESS_TABLE_INPUTS_COPY_CONSTRUCTORS_H

inline int &getCopyConstructorCount() {
  static int count = 0;
  return count;
}

struct __attribute__((swift_attr("import_unsafe")))
HasUserProvidedCopyConstructor {
  int numCopies;
  HasUserProvidedCopyConstructor(int numCopies = 0) : numCopies(numCopies) {}
  HasUserProvidedCopyConstructor(const HasUserProvidedCopyConstructor &other)
      : numCopies(other.numCopies + 1) {
    ++getCopyConstructorCount();
  }
};

inline int readWithArgument(const HasUserProvidedCopyConstructor &value,
                            const int *argument) {
  return value.numCopies + *argument;
}

struct HasNonTrivialImplicitCopyConstructor {
  HasUserProvidedCopyConstructor box;
  HasNonTrivialImplicitCopyConstructor()
      : box(HasUserProvidedCopyConstructor()) {}
};

struct HasNonTrivialDefaultCopyConstructor {
  HasUserProvidedCopyConstructor box;
  HasNonTrivialDefaultCopyConstructor()
      : box(HasUserProvidedCopyConstructor()) {}
  HasNonTrivialDefaultCopyConstructor(
      const HasNonTrivialDefaultCopyConstructor &) = default;
};

struct HasCopyConstructorWithDefaultArgs {
  int value;
  HasCopyConstructorWithDefaultArgs(int value) : value(value) {}

  HasCopyConstructorWithDefaultArgs(
      const HasCopyConstructorWithDefaultArgs &other, int value = 1)
      : value(other.value + value) {}

  HasCopyConstructorWithDefaultArgs(HasCopyConstructorWithDefaultArgs &&) =
      default;
};

struct HasCopyConstructorWithOneParameterWithDefaultArg {
  int numCopies;

  HasCopyConstructorWithOneParameterWithDefaultArg(int numCopies)
      : numCopies(numCopies) {}

  HasCopyConstructorWithOneParameterWithDefaultArg(
      const HasCopyConstructorWithOneParameterWithDefaultArg &other =
          HasCopyConstructorWithOneParameterWithDefaultArg{1})
      : numCopies(other.numCopies + 1) {}
};

// Make sure that we don't crash on struct templates with copy-constructors.
template <typename T> struct S {
  S(S const &) {}
};

#endif // TEST_INTEROP_CXX_VALUE_WITNESS_TABLE_INPUTS_COPY_CONSTRUCTORS_H
