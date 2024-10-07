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
