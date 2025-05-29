class MyClass {
private:
  int value;

public:
  MyClass(int v) : value(v) {}
  MyClass() : MyClass(0) {}

  void printValue() const {}
};
