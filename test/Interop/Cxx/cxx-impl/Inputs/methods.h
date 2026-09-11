#ifndef TEST_INTEROP_CXX_CXX_IMPL_METHODS_H
#define TEST_INTEROP_CXX_CXX_IMPL_METHODS_H

// Static, const, and non-const methods of a value type

struct Counter {
  int value;

  static Counter make(int v);
  int get() const;
  void add(int d);
  int overloadedByArity() const;
  int overloadedByArity(int x) const;
  int renamedTarget() const;
};

// A const and a non-const overload have the same parameter types. The importer
// suffixes the Swift name of the non-const overload with `Mutating`.

struct Pair {
  int value;

  int adjust(int x) const;
  int adjust(int x);
  int adjust(int x, int y);
};

// A struct too large to be returned in registers is returned indirectly

struct Triple {
  long a;
  long b;
  long c;
};

struct Holder {
  int value;

  Triple spread(int k) const;
  static Triple makeTriple(long a);
};

// A non-trivial receiver is passed by pointer, unlike a non-trivial parameter
// or result

class NonTrivialReceiver {
public:
  NonTrivialReceiver() {}
  NonTrivialReceiver(const NonTrivialReceiver &other) : value(other.value) {}
  ~NonTrivialReceiver() {}
  int value;

  int read() const;
  void write(int v);
};

// Rejected matches

struct Rejections {
  int value;

  int constMethod() const;
  void nonConstMethod();
  int instanceMethod() const;
  static int staticMethod();
  inline int inlineMethod() const { return value; }
};

struct Polymorphic {
  virtual int virtualMethod() const;
  int nonVirtualMethod() const;
};

// Inherited members belong to the base

struct Base {
  int baseMethod() const;
};

struct Derived : Base {
  int derivedMethod() const;
};

// Foreign reference type

struct Widget;
void retainWidget(Widget *);
void releaseWidget(Widget *);

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainWidget")))
__attribute__((swift_attr("release:releaseWidget"))) Widget {
  int id;

  int tag() const;
  virtual int describe() const;
  static int count();
};

#endif // !TEST_INTEROP_CXX_CXX_IMPL_METHODS_H
