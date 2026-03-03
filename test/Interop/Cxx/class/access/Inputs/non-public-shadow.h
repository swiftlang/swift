#ifndef NON_PUBLIC_SHADOW_H
#define NON_PUBLIC_SHADOW_H

/// Used to distinguish which member we resolve to.
enum class Return {
  Publ_publOrPriv,
  Publ_publOrProt,
  Publ_publPrivShadowed,
  Publ_publPublShadowed,

  Prot_publOrProt,
  Prot_protOrPriv,
  Prot_protPrivShadowed,
  Prot_protPublShadowed,

  Priv_publOrPriv,
  Priv_protOrPriv,
  Priv_privPrivShadowed,
  Priv_privPublShadowed,

  Shadow_publPrivShadowed,
  Shadow_publPublShadowed,
  Shadow_protPrivShadowed,
  Shadow_protPublShadowed,
  Shadow_privPrivShadowed,
  Shadow_privPublShadowed,
};

struct Publ {
public:
  Return publOrPriv(void) const { return Return::Publ_publOrPriv; }
  Return publOrProt(void) const { return Return::Publ_publOrProt; }
  Return publPrivShadowed(void) const { return Return::Publ_publPrivShadowed; }
  Return publPublShadowed(void) const { return Return::Publ_publPublShadowed; }
};

struct Prot {
protected:
  Return publOrProt(void) const { return Return::Prot_publOrProt; }
  Return protOrPriv(void) const { return Return::Prot_protOrPriv; }
  Return protPrivShadowed(void) const { return Return::Prot_protPrivShadowed; }
  Return protPublShadowed(void) const { return Return::Prot_protPublShadowed; }
};

struct Priv {
private:
  Return publOrPriv(void) const { return Return::Priv_publOrPriv; }
  Return protOrPriv(void) const { return Return::Priv_protOrPriv; }
  Return privPrivShadowed(void) const { return Return::Priv_privPrivShadowed; }
  Return privPublShadowed(void) const { return Return::Priv_privPublShadowed; }
};

struct Shadow : Priv, Prot, Publ {
public:
  Return publPublShadowed(void) const {
    return Return::Shadow_publPublShadowed;
  }
  Return protPublShadowed(void) const {
    return Return::Shadow_protPublShadowed;
  }
  Return privPublShadowed(void) const {
    return Return::Shadow_privPublShadowed;
  }

private:
  Return publPrivShadowed(void) const {
    return Return::Shadow_publPrivShadowed;
  }
  Return protPrivShadowed(void) const {
    return Return::Shadow_protPrivShadowed;
  }
  Return privPrivShadowed(void) const {
    return Return::Shadow_privPrivShadowed;
  }
};

#endif /* NON_PUBLIC_SHADOW_H */
