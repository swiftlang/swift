inline int notCalled() {
  return 42;
}

inline int calledIndirectly() {
  return 42;
}

inline int calledDirectly() {
  return calledIndirectly();
}
