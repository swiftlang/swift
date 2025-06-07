class E;

class C {
public:
  E *getE() const;
};

class D : public C {};

class E : public D {
public:
  D *getD() const;
};
