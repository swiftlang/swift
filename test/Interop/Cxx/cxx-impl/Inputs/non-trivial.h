#ifndef TEST_INTEROP_CXX_CXX_IMPL_NON_TRIVIAL_H
#define TEST_INTEROP_CXX_CXX_IMPL_NON_TRIVIAL_H

// A class with a user-provided copy constructor and destructor. The execution
// test defines them to count live objects and copies.
struct Tracked {
  int value;
  Tracked(int v);
  Tracked(const Tracked &other);
  ~Tracked();
  static int liveCount;
  static int copyCount;
};

// A class with a move constructor. Swift only ever copies; C++ callers may
// move into the argument temporary.
struct Movable {
  int value;
  Movable(int v);
  Movable(const Movable &other);
  Movable(Movable &&other);
  ~Movable();
  static int liveCount;
};

// A move-only class; it imports as `~Copyable`.
struct MoveOnly {
  int value;
  MoveOnly(int v);
  MoveOnly(MoveOnly &&other);
  MoveOnly(const MoveOnly &) = delete;
  ~MoveOnly();
  static int liveCount;
};

// A polymorphic class passed by value. The execution test defines the key
// function (the destructor) and so emits the vtable.
struct Polymorphic {
  int value;
  Polymorphic(int v);
  Polymorphic(const Polymorphic &other);
  virtual ~Polymorphic();
  virtual int tag() const;
};

// By value.

int takesTracked(Tracked t);
int takesTwoTracked(Tracked a, Tracked b);
int copiesTracked(Tracked t);
Tracked returnsTracked(int v);
Tracked passesThroughTracked(Tracked t);

int takesMovable(Movable m);
Movable returnsMovable(int v);

MoveOnly returnsMoveOnly(int v);

int takesPolymorphic(Polymorphic p);

// Methods.

struct Box {
  int base;
  int take(Tracked t) const;
  int add(Tracked t);
  Tracked produce() const;
  static Tracked wrap(int v);
};

// By reference (implemented with the pointer spelling).

int readTracked(const Tracked &t);
void bumpTracked(Tracked &t);
void assignTracked(Tracked &dst, const Tracked &src);

// Rejected: implementations spelled `borrowing` or `consuming`. A move-only
// class cannot be taken by value at all, since its parameter needs one of them.

int takesTrackedBorrowing(Tracked t);
int takesTrackedConsuming(Tracked t);
int takesMoveOnlyBorrowing(MoveOnly m);
int takesMoveOnlyConsuming(MoveOnly m);

#endif
