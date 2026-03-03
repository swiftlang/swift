#pragma once

#include <memory>

struct SharableFromThis : std::enable_shared_from_this<SharableFromThis> {
  int field = 42;
};

struct Foo {};
using MalformedFoo = std::enable_shared_from_this<Foo>;
using MalformedInt = std::enable_shared_from_this<int>;
