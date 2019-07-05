#pragma once

// Ensure c++ features are used.
namespace ns {
class T {};
class NamespacedType {};
} // namespace ns

struct Basic {
  int a;
  ns::T *b;
};

Basic makeA();

ns::T* makeT();
void useT(ns::T* v);

using namespacedT = ns::T;
using ns::NamespacedType;
