template <class T> T add(T a, T b) { return a + b; }

template <class A, class B> A addTwoTemplates(A a, B b) { return a + b; }

template <class T> T passThrough(T value) { return value; }

template <class T> const T passThroughConst(const T value) { return value; }

void takesString(const char *) {}
template <class T> void expectsString(T str) { takesString(str); }

template <long x> void integerTemplate() {}
template <long x = 0> void defaultIntegerTemplate() {}

// We cannot yet use this in swift but, make sure we don't crash when parsing
// it.
template <class R, class T, class U> R returns_template(T a, U b) {
  return a + b;
}

// Same here:
template <class T> void cannot_infer_template() {}
