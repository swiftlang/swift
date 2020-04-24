int counterWrapper();

inline int counter() {
  static int a = 0;
  return a++;
}
