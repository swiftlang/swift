int counter = 0;

int getCounterFromCxx() {
  return counter;
}

void setCounterFromCxx(int c) {
  counter = c;
}

namespace Namespaced {
  int counter = 0;

  int getCounterFromCxx() {
    return counter;
  }

  void setCounterFromCxx(int c) {
    counter = c;
  }
}
