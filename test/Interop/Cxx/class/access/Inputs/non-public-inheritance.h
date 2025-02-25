#ifndef NON_PUBLIC_INHERITANCE_H
#define NON_PUBLIC_INHERITANCE_H

#define BLESS                                                                  \
  __attribute__((__swift_attr__("private_fileid:main/blessed.swift")))

const int PUBL_RETURN_VAL = 1;
const int PROT_RETURN_VAL = 2;
const int PRIV_RETURN_VAL = 3;

class BLESS Base {
public:
int publ(void) const { return PUBL_RETURN_VAL; }

protected:
int prot(void) const { return PROT_RETURN_VAL; }

private:
int priv(void) const { return PRIV_RETURN_VAL; }
};

class BLESS PublBase : public Base {};
class BLESS ProtBase : protected Base {};
class BLESS PrivBase : private Base {};

class BLESS PublPublBase : public PublBase {};
class BLESS ProtPublBase : protected PublBase {};
class BLESS PrivPublBase : private PublBase {};

class BLESS PublProtBase : public ProtBase {};
class BLESS ProtProtBase : protected ProtBase {};
class BLESS PrivProtBase : private ProtBase {};

class BLESS PublPrivBase : public PrivBase {};
class BLESS ProtPrivBase : protected PrivBase {};
class BLESS PrivPrivBase : private PrivBase {};

#endif /* NON_PUBLIC_INHERITANCE_H */
