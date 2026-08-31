#ifndef CXX_MOVE_ONLY_H
#define CXX_MOVE_ONLY_H

extern "C" void noteMade(void);
extern "C" void noteDtor(void);
extern "C" void noteDoubleFree(void);

/// A C++ move-only type: copy construction and assignment are deleted, and it
/// has a non-trivial destructor. Swift imports this as `~Copyable`.
///
/// Both the default and move constructors count as "made", so a correct program
/// destroys exactly as many objects as it makes. The destructor poisons the
/// pointer so that destroying the same object twice is detected rather than
/// silently double-freeing.
struct MoveOnly {
  int *p;

  MoveOnly() : p(new int(7)) { noteMade(); }
  MoveOnly(const MoveOnly &) = delete;
  MoveOnly &operator=(const MoveOnly &) = delete;
  MoveOnly(MoveOnly &&other) : p(other.p) {
    other.p = nullptr;
    noteMade();
  }
  ~MoveOnly() {
    noteDtor();
    if (p == poison())
      noteDoubleFree();
    delete p;
    p = poison();
  }

  int value() const { return p ? *p : -1; }

private:
  static int *poison() { return reinterpret_cast<int *>(-1); }
};

#endif
