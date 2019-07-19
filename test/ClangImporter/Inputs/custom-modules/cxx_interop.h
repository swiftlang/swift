#pragma once

// Ensure c++ features are used.
namespace ns {
class T {};
} // namespace ns

struct Basic {
  int a;
  ns::T *b;
};

Basic makeA();
