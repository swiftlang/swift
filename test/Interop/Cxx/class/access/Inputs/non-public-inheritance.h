#ifndef NON_PUBLIC_INHERTIANCE_H
#define NON_PUBLIC_INHERTIANCE_H

#define BLESS __attribute__((__swift_attr__("private_fileid:main/blessed.swift")))

class BLESS Base {
public:
  int publ(void) const { return 1; }
protected:
  int prot(void) const { return 2; }
private:
  int priv(void) const { return 3; }
};

class BLESS PublBase : public    Base {};
class BLESS ProtBase : protected Base {};
class BLESS PrivBase : private   Base {};

class BLESS PublPublBase : public    PublBase {};
class BLESS ProtPublBase : protected PublBase {};
class BLESS PrivPublBase : private   PublBase {};

class BLESS PublProtBase : public    ProtBase {};
class BLESS ProtProtBase : protected ProtBase {};
class BLESS PrivProtBase : private   ProtBase {};

class BLESS PublPrivBase : public    PrivBase {};
class BLESS ProtPrivBase : protected PrivBase {};
class BLESS PrivPrivBase : private   PrivBase {};

#endif /* NON_PUBLIC_INHERTIANCE_H */
