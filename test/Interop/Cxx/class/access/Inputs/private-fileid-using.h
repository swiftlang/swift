#ifndef PRIVATE_FILEID_USING_H
#define PRIVATE_FILEID_USING_H

#ifndef BLESSED
#define BLESSED __attribute__((__swift_attr__("private_fileid:main/blessed.swift")))
#endif

class Base {
protected:
  int protField = 1;
  int protFieldNoUsing = 2;
  int protFieldProtUsing = 4;
  void protMethod(void) const {}

public:
  int publField = 3;
  void publMethod(void) const {}
};

class BLESSED PublUser : public Base {
public:
  using Base::protField;
  using Base::protMethod;
};

class BLESSED PrivUser : private Base {
public:
  using Base::protField;
  using Base::publField;
  using Base::protMethod;
  using Base::publMethod;

protected:
  using Base::protFieldProtUsing;
};

class BLESSED PublPrivUser : public PrivUser {};

#endif // PRIVATE_FILEID_USING_H
