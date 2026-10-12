#ifndef TEST_INTEROP_CXX_CXX_IMPL_VIRTUAL_H
#define TEST_INTEROP_CXX_CXX_IMPL_VIRTUAL_H

// The key function is implemented in Swift, so Swift emits the vtable and RTTI.

struct Shape {
  int sides;

  // The key function.
  virtual int area() const;
  virtual void scale(int factor);
  // Emitted along with the vtable, which names it.
  virtual int perimeter() const { return 4 * sides; }
};

// The key functions stay in C++, so Swift emits no vtables. The override needs
// no thunk.

struct SimpleBase {
  int stored;

  // The key function; its body stays in C++ (the execution test's main file).
  virtual void sbAnchor();
  virtual int simple() const;
};
struct SimpleDerived : SimpleBase {
  // The key function; its body stays in C++ (the execution test's main file).
  virtual void sdAnchor();
  int simple() const override;
};

// A pure virtual method cannot be implemented; the key function can.

struct Abstract {
  // The key function.
  virtual int anchor() const;
  // expected-note@+1{{'pureMethod' declared pure virtual here}}
  virtual int pureMethod() const = 0;
};

// A covariant return to a base at a nonzero offset needs a return-adjusting
// thunk.

struct RetA {
  int a;
};
struct RetB {
  int b;
};
struct RetC : RetA, RetB {};

RetC *_Nonnull sharedRetC();

struct CloneBase {
  virtual RetB *_Nonnull clone();
};
struct CloneDerived : CloneBase {
  // The key function; its body stays in C++ (the execution test's main file).
  virtual void cloneAnchor();
  RetC *_Nonnull clone() override;
};

// Multiple inheritance: overriding the non-primary base's method needs a
// this-adjusting thunk. Swift also emits the implicit destructor.

extern int destroyedMIBaseA;

struct MIBaseA {
  int a;

  virtual ~MIBaseA() { ++destroyedMIBaseA; }
  virtual void firstA();
};
struct MIBaseB {
  int b;

  virtual int fromB() const;
};
struct MIDerived : MIBaseA, MIBaseB {
  // The key function.
  virtual void miAnchor();
  void firstA() override;
  int fromB() const override;
};

// Virtual inheritance: the override needs a vcall-offset thunk, and Swift emits
// the VTT.

struct VBase {
  int vb;

  virtual int vbMethod() const;
};
struct VDerived : virtual VBase {
  int vd;

  // The key function.
  virtual void vAnchor();
  int vbMethod() const override;
};

// A foreign reference type: Swift calls dispatch through the importer's thunk.

struct Engine;
void retainEngine(Engine *_Nonnull);
void releaseEngine(Engine *_Nonnull);

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainEngine")))
__attribute__((swift_attr("release:releaseEngine"))) Engine {
  int rpm;

  // The key function.
  virtual int status() const;
  virtual void boost(int amount);
};

// A pure virtual method of a foreign reference type.

struct AbstractEngine;
void retainAbstractEngine(AbstractEngine *_Nonnull);
void releaseAbstractEngine(AbstractEngine *_Nonnull);

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainAbstractEngine")))
__attribute__((swift_attr("release:releaseAbstractEngine"))) AbstractEngine {
  // The key function.
  virtual int aeAnchor() const;
  // expected-note@+1{{'pureStatus' declared pure virtual here}}
  virtual int pureStatus() const = 0;
};

// Overloaded virtual methods: every overload has its own vtable slot, so Swift
// can implement one overload while another stays in C++.

struct Mixer {
  int level;

  // The key function; its body stays in C++ (the execution test's main file).
  virtual void mixerAnchor();
  virtual int mix(int amount) const;
  virtual int mix(double amount) const;
};

struct Gauge;
void retainGauge(Gauge *_Nonnull);
void releaseGauge(Gauge *_Nonnull);

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainGauge")))
__attribute__((swift_attr("release:releaseGauge"))) Gauge {
  int level;

  // The key function; its body stays in C++ (the execution test's main file).
  virtual void gaugeAnchor();
  virtual int read(int scale) const;
  virtual int read(double scale) const;
};

#endif // !TEST_INTEROP_CXX_CXX_IMPL_VIRTUAL_H
