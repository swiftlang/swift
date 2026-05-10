#ifndef TEST_INTEROP_CXX_CLASS_INHERITANCE_POLYMORPHISM_H_
#define TEST_INTEROP_CXX_CLASS_INHERITANCE_POLYMORPHISM_H_

class Shape {
 public:
  virtual int NumberOfSides() { return 0; }
};

class Rectangle : public Shape {
 public:
  virtual int NumberOfSides() { return 4; }
};

// For testing runtime polymorphism.
Shape* MakeShape() {
  return new Rectangle();
}

#endif  // TEST_INTEROP_CXX_CLASS_INHERITANCE_POLYMORPHISM_H_

