#ifndef _USING_BASE_MEMBERS_H
#define _USING_BASE_MEMBERS_H

struct PublicBase {
private:
  int value = 123;

public:
  int publicGetter() const { return value; }
  void publicSetter(int v) { value = v; }
  void notExposed() const {}
};

struct PublicBasePrivateInheritance : private PublicBase {
  using PublicBase::publicGetter;
  using PublicBase::publicSetter;
};

struct PublicBaseProtectedInheritance : protected PublicBase {
  using PublicBase::publicGetter;
  using PublicBase::publicSetter;
};

struct PublicBaseUsingPrivateTypedef : private PublicBase {
private:
  typedef PublicBase MyBase;
public:
  using MyBase::publicGetter;
  using MyBase::publicSetter;
};

struct PublicBaseUsingPrivateUsingType : private PublicBase {
private:
  using MyBase = PublicBase;
public:
  using MyBase::publicGetter;
  using MyBase::publicSetter;
};

struct IntBox {
  int value;
  IntBox(int value) : value(value) {}
  IntBox(unsigned value) : value(value) {}
};

struct UsingBaseConstructorWithParam : IntBox {
  using IntBox::IntBox;
};

struct Empty {};

struct UsingBaseConstructorEmpty : private Empty {
  using Empty::Empty;

  int value = 456;
};

struct ProtectedBase {
protected:
  int protectedGetter() const { return 456; }
};

struct ProtectedMemberPrivateInheritance : private ProtectedBase {
  using ProtectedBase::protectedGetter;
};

struct OperatorBase {
  operator bool() const { return true; }
  int operator*() const { return 456; }
  OperatorBase operator!() const { return *this; }
  // int operator[](const int x) const { return x; } // FIXME: see below
};

struct OperatorBasePrivateInheritance : private OperatorBase {
public:
  using OperatorBase::operator bool;
  using OperatorBase::operator*;
  using OperatorBase::operator!;
  // using OperatorBase::operator[];  // FIXME: using operator[] is broken
};

#endif // !_USING_BASE_MEMBERS_H
