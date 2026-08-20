#ifndef TEST_INTEROP_CXX_CXX_IMPL_VIRTUAL_H
#define TEST_INTEROP_CXX_CXX_IMPL_VIRTUAL_H

// A simple polymorphic class.

struct Shape {
  int sides;

  // The key function: the first out-of-line, non-pure virtual method. Its body
  // must stay in C++; it anchors the vtable that dispatches to the
  // Swift-implemented methods below.
  virtual int keyFunction() const;
  virtual int area() const;
  virtual void scale(int factor);
};

// Along a single-inheritance chain with an unchanged return type, every
// overridden slot sits at offset zero: no vtable entry can need an adjusting
// thunk, and the override is accepted.

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

// A pure virtual method's vtable slot dispatches to an overriding method,
// never to a definition of the method itself.

struct Abstract {
  virtual int anchor() const;
  virtual int pureMethod() const = 0;
};

// A covariant return type crossing to a base at a nonzero offset makes the
// overridden slot need a return-adjusting thunk.

struct RetA {
  int a;
};
struct RetB {
  int b;
};
struct RetC : RetA, RetB {};

struct CloneBase {
  virtual RetB *_Nonnull clone();
};
struct CloneDerived : CloneBase {
  virtual void cloneAnchor();
  RetC *_Nonnull clone() override;
};

// An override of a method of a non-primary base needs a this-adjusting thunk
// in that base's secondary vtable; under multiple inheritance even an
// override of the primary base's method is conservatively rejected.

struct MIBaseA {
  int a;
  virtual void firstA();
};
struct MIBaseB {
  int b;
  virtual int fromB() const;
};
struct MIDerived : MIBaseA, MIBaseB {
  virtual void miAnchor();
  void firstA() override;
  int fromB() const override;
};

// An override of a method of a virtual base needs a this-adjusting thunk with
// a virtual (vcall-offset) adjustment.

struct VBase {
  int vb;
  virtual int vbMethod() const;
};
struct VDerived : virtual VBase {
  virtual void vAnchor();
  int vbMethod() const override;
};

// A foreign reference type: Swift calls dispatch dynamically through the
// importer's synthesized thunk, while the Swift implementation provides the
// body the vtable slot names.

struct Engine;
void retainEngine(Engine *_Nonnull);
void releaseEngine(Engine *_Nonnull);

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainEngine")))
__attribute__((swift_attr("release:releaseEngine"))) Engine {
  int rpm;

  // The key function; its body stays in C++ (the execution test's main file).
  virtual void keyAnchor();
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
  virtual void aeAnchor();
  virtual int pureStatus() const = 0;
};

#endif // !TEST_INTEROP_CXX_CXX_IMPL_VIRTUAL_H
