class AbstractBase {
public:
  AbstractBase(int x) : _x(x) {}
  virtual ~AbstractBase() {}

  int getValue() const { return _x; }
  int callsPureVirtual() const { return pureVirtual(); }

  virtual int pureVirtual() const = 0;

private:
  int _x;
};

class ConcreteDerived : public AbstractBase {
public:
  ConcreteDerived() : AbstractBase(42) {}
  int pureVirtual() const override { return 123; }
  ~ConcreteDerived() {}
};

class StillAbstractDerived : public AbstractBase {
public:
  StillAbstractDerived() : AbstractBase(0) {}
};
