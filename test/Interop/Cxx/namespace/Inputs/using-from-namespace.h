#pragma once

namespace Test {
  class Foo {};
  namespace Test2 {
  class Bar {};
  } // namespace Test2

  using Test2::Bar;
} // namespace Test

using Test::Bar;
using Test::Foo;
