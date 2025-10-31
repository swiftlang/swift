#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_UNIQUE_PTR_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_UNIQUE_PTR_H

#include <memory>
#include <expected>

using NonCopyableExpected = std::expected<std::unique_ptr<bool>, int>;

template<typename T>
class UniqueRef {
public:
  std::unique_ptr<T> _field;
};

struct Decoder {};
enum Error {
  DoomA,
  DoomB
};

using DecoderOrError = std::expected<UniqueRef<Decoder>, Error>;

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_UNIQUE_PTR_H
