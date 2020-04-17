int counter = 0;

int count() {
  return ++counter;
}

namespace Namespaced {
  int counter = 0;

  int count() {
    return ++counter;
  }
}
