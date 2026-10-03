#ifndef TEST_INTEROP_CXX_CXX_IMPL_FOREIGN_REFERENCE_VIRTUAL_H
#define TEST_INTEROP_CXX_CXX_IMPL_FOREIGN_REFERENCE_VIRTUAL_H

// Overrides in foreign reference types, which import as Swift subclasses.

struct Base;
void retainBase(Base *_Nonnull);
void releaseBase(Base *_Nonnull);

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainBase")))
__attribute__((swift_attr("release:releaseBase"))) Base {
  int value;

  // The key function, in Swift.
  virtual int anchor() const;
  // Overridden by Derived, Leaf, and MultiDerived.
  virtual int describe() const;
  // Overridden by Leaf, not by Derived.
  virtual int tag() const;
  // Hidden by Derived::hide.
  int hide() const;
};

// The override is the key function.
struct Derived : Base {
  int describe() const override;
  int hide() const;
};

// Overrides a method Derived overrides, and one it only inherits.
struct Leaf : Derived {
  int describe() const override;
  int tag() const override;
};

// A non-primary base, so its method's override needs a this-adjusting thunk.
struct SecondBase {
  int second;

  // The key function, in C++.
  virtual int fromSecond() const;
};

struct MultiDerived : Base, SecondBase {
  int describe() const override;
  int fromSecond() const override;
};

// An override of a pure virtual method.

struct AbstractBase;
void retainAbstractBase(AbstractBase *_Nonnull);
void releaseAbstractBase(AbstractBase *_Nonnull);

struct __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:retainAbstractBase")))
__attribute__((swift_attr("release:releaseAbstractBase"))) AbstractBase {
  // The key function, in C++.
  virtual int abstractAnchor() const;
  virtual int pure() const = 0;
};

// The override is the key function.
struct ConcreteDerived : AbstractBase {
  int pure() const override;
};

// Value types, whose inheritance is not Swift inheritance.

struct ValueBase {
  // The key function.
  virtual int valueAnchor() const;
  virtual int get() const;
};

struct ValueDerived : ValueBase {
  int get() const override;
};

#endif // !TEST_INTEROP_CXX_CXX_IMPL_FOREIGN_REFERENCE_VIRTUAL_H
