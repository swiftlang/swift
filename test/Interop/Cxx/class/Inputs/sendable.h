struct HasPrivatePointerField {
private:
  const int *ptr;
};

struct HasProtectedPointerField {
protected:
  const int *ptr;
};

struct HasPublicPointerField {
  const int *ptr;
};

struct HasPrivateNonSendableField {
private:
  HasPrivatePointerField f;
};

struct HasProtectedNonSendableField {
protected:
  HasProtectedPointerField f;
};

struct HasPublicNonSendableField {
  HasPublicPointerField f;
};

struct DerivedFromHasPublicPointerField : HasPublicPointerField {};
struct DerivedFromHasPublicNonSendableField : HasPublicNonSendableField {};
struct DerivedFromHasPrivatePointerField : HasPrivatePointerField {};

struct DerivedPrivatelyFromHasPublicPointerField : private HasPublicPointerField {};
struct DerivedPrivatelyFromHasPublicNonSendableField : private HasPublicNonSendableField {};
struct DerivedPrivatelyFromHasPrivatePointerField : private HasPrivatePointerField {};

struct DerivedProtectedFromHasPublicPointerField : protected HasPublicPointerField {};
struct DerivedProtectedFromHasPublicNonSendableField : protected HasPublicNonSendableField {};
struct DerivedProtectedFromHasPrivatePointerField : protected HasPrivatePointerField {};
