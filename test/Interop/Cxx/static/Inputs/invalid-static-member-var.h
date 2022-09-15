template<class T>
struct Bad {
  typename T::doesnotexist x;
};

struct X {
  static Bad<int> b;
};
