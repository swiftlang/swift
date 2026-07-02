#pragma once
#include <vector>
#include <swift/bridging>

struct ImmortalNode {
  int value;
} SWIFT_IMMORTAL_REFERENCE;

struct ImmortalNode2 {
  int value;
} SWIFT_IMMORTAL_REFERENCE;

struct SharedNode {
  int value;
  mutable int retainCount = 1;

  SharedNode(int v) : value(v), retainCount(1) {}
  SharedNode(const SharedNode &o) : value(o.value), retainCount(1) {}

  void retain() const { ++retainCount; }
  void release() const { --retainCount; }
} SWIFT_SHARED_REFERENCE(.retain, .release);

inline std::vector<ImmortalNode *> makeImmortalPtrVector() {
  static ImmortalNode a{10}, b{20}, c{30};
  return {&a, &b, &c};
}

inline std::vector<SharedNode *> makeSharedPtrVector() {
  return {new SharedNode(101), new SharedNode(202), new SharedNode(303)};
}

// Use a distinct type from ImmortalNode to avoid a known duplicate witness
// table bug when vector<T> and vector<T*> coexist for the same T.
inline std::vector<ImmortalNode2> makeImmortalValVector() {
  return {{11}, {22}, {33}};
}

// Custom container whose methods return raw pointers to an FRT, which get
// imported as Optional<FRT> and cannot be iterated over.
struct RawPtrIterContainer {
  ImmortalNode *data;
  int size;

  RawPtrIterContainer() {
    static ImmortalNode storage[3] = {{10}, {20}, {30}};
    data = storage;
    size = 3;
  }

  const ImmortalNode *begin() const { return data; }
  const ImmortalNode *end() const { return data + size; }
  const ImmortalNode *operator[](int i) const { return &data[i]; }
};
