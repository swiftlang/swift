#pragma once

struct BoolBox {
  bool value;

  void set(bool v) { value = v; }

  operator bool() const { return value; }
};

struct NonConstBoolBox {
  bool value;

  operator bool() { return value; }
};

struct DualOverloadBoolBox {
  bool value;

  operator bool() const { return value; }
  operator bool() { return value; }
};

struct ExplicitBoolBox {
private:
  bool value;

public:
  explicit operator bool() const { return value; }
};

struct BoolBoxWithOtherConversions {
  int value;
  operator bool() const { return value != 0; }
  operator int() const { return value; }
};

struct DeletedBoolBox {
  operator bool() const = delete;
};

struct InheritedBoolBox : BoolBox {};

struct OverriddenBoolBox : BoolBox {
  operator bool() const { return !value; }
};

struct AnotherBoolBase {
  operator bool() const { return true; }
};

struct AmbiguousBoolBox : BoolBox, AnotherBoolBase {};

struct VirtualBoolLeft : virtual BoolBox {};
struct VirtualBoolRight : virtual BoolBox {};
struct VirtualDiamondBoolBox : VirtualBoolLeft, VirtualBoolRight {};

struct NonVirtualBoolLeft : BoolBox {};
struct NonVirtualBoolRight : BoolBox {};
struct DiamondBoolBox : NonVirtualBoolLeft, NonVirtualBoolRight {};

struct PrivateBoolBox {
private:
  operator bool() const { return true; }
};

struct ProtectedBoolBox {
protected:
  operator bool() const { return true; }
};

struct PrivateInheritedBoolBox : private BoolBox {};
struct ProtectedInheritedBoolBox : protected BoolBox {};

struct PublicUsingBoolBox : ProtectedBoolBox {
  using ProtectedBoolBox::operator bool;
};
struct ProtectedUsingBoolBox : BoolBox {
protected:
  using BoolBox::operator bool;
};
