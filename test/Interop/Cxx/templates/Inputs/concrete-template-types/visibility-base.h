#pragma once
namespace visibility {
template <class T>
struct Box {
  int value;
};
template struct Box<int>;
} // namespace visibility
