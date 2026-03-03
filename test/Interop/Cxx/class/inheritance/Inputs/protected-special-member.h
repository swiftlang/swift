#pragma once
#include <iostream>
#include <vector>

// NOTE: expected notes are suppressed due to -suppress-notes.

// suppressed-note@+1 {{record 'ProtectedDtor' is not automatically available}}
struct ProtectedDtor {
  int fromBase = 111;
protected:
  ~ProtectedDtor() { std::cout << "~ProtectedDtor(fromBase = " << fromBase << ")\n"; }
};

struct InheritsProtectedDtor : ProtectedDtor {
  int inDerived = 222;
};

struct PrivatelyInheritsProtectedDtor : private ProtectedDtor {
  int inDerived = 222;
  void setFromBase(int v) { fromBase = v; }
};

// suppressed-note@+1 {{record 'ProtectedDtorField' is not automatically available}}
struct ProtectedDtorField {
  int inDerived = 222;
  ProtectedDtor pd{};
};

// suppressed-note@+1 {{record 'ProtectedDtorBaseAndField' is not automatically available}}
struct ProtectedDtorBaseAndField : ProtectedDtor {
  int inDerived = 222;
  ProtectedDtor pd{};
};

// Note that ProtectedCopy is not imported because it does not define a move
// constructor, which means it is neither copyable nor move-only.
// suppressed-note@+1 {{record 'ProtectedCopy' is not automatically available}}
struct ProtectedCopy {
  int fromBase = 111;
  ProtectedCopy() = default;
protected:
  // NOTE: the copy has an incremented fromBase, so we can check at runtime that
  // the correct custom copy operation took place
  ProtectedCopy(const ProtectedCopy &c) : fromBase{c.fromBase + 1} {}
};

struct InheritsProtectedCopy : ProtectedCopy {
  int getFromBase(void) const { return fromBase; }
  void setFromBase(int x) { fromBase = x; }
};

struct PrivatelyInheritsProtectedCopy : private ProtectedCopy {
  int getFromBase(void) const { return fromBase; }
  void setFromBase(int x) { fromBase = x; }
};

struct PrivatelyInheritsPrivatelyInheritsProtectedCopy : private PrivatelyInheritsProtectedCopy {};

// suppressed-note@+1 {{record 'ProtectedCopyField' is not automatically available}}
struct ProtectedCopyField {
  ProtectedCopy pc;
};

// suppressed-note@+1 {{record 'ProtectedCopyBaseAndField' is not automatically available}}
struct ProtectedCopyBaseAndField : ProtectedCopy {
  ProtectedCopy pc;
};

using VecOfProtectedCopy = std::vector<ProtectedCopy>;
using VecOfInheritsProtectedCopy = std::vector<InheritsProtectedCopy>;

// suppressed-note@+1 {{record 'FieldVecOfProtectedCopy ' is not automatically available}}
struct FieldVecOfProtectedCopy { std::vector<ProtectedCopy> v; };
struct FieldVecOfInheritsProtectedCopy {
  std::vector<InheritsProtectedCopy> v;
  FieldVecOfInheritsProtectedCopy(int x, int y, int z) : v{} {
    v.emplace_back(InheritsProtectedCopy{});
    v.emplace_back(InheritsProtectedCopy{});
    v.emplace_back(InheritsProtectedCopy{});
    v[0].setFromBase(x);
    v[1].setFromBase(y);
    v[2].setFromBase(z);
  }
  int get(unsigned idx) const { return v[idx].getFromBase(); }
};

// suppressed-note@+1 {{record 'ProtectedMove' is not automatically available}}
struct ProtectedMove {
  int fromBase = 111;
  ProtectedMove() = default;
protected:
  ProtectedMove(ProtectedMove &&) = default;
};

struct InheritsProtectedMove : ProtectedMove {
  int getFromBase(void) const { return fromBase; }
  void setFromBase(int x) { fromBase = x; }
};

using VecOfProtectedMove = std::vector<ProtectedMove>;
using VecOfInheritsProtectedMove = std::vector<InheritsProtectedMove>;

// suppressed-note@+1 {{record 'FieldVecOfProtectedMove ' is not automatically available}}
struct FieldVecOfProtectedMove { std::vector<ProtectedMove> v; };
struct FieldVecOfInheritsProtectedMove { std::vector<InheritsProtectedMove> v; };

struct ProtectedCopyWithMove {
  int fromBase = 111;
  ProtectedCopyWithMove() = default;
  ProtectedCopyWithMove(ProtectedCopyWithMove &&) = default;
protected:
  // NOTE: the copy has an incremented fromBase, so we can check at runtime that
  // the correct custom copy operation took place
  ProtectedCopyWithMove(const ProtectedCopyWithMove &o) : fromBase{o.fromBase + 1} {}
};

struct InheritsProtectedCopyWithMove : ProtectedCopyWithMove {
  int getFromBase(void) const { return fromBase; }
  void setFromBase(int x) { fromBase = x; }
};

struct ProtectedCopyWithMoveField {
  ProtectedCopyWithMove pcm;
};

struct ProtectedCopyWithMoveBaseAndField : ProtectedCopyWithMove {
  ProtectedCopyWithMove pcm;
};
