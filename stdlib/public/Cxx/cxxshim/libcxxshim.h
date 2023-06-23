template <class From, class To>
To __swift_interopStaticCast(From from) {
  return static_cast<To>(from);
}

template <class T, long N>
struct __attribute__((swift_attr("conforms_to:Cxx._UnsafeCxxStaticArray")))
_UnsafeCxxStaticArrayImpl {
  using Element = T;

  inline const Element *_Nonnull data() const { return storage; }

  inline long getCount() { return N; }

private:
  Element storage[N];
};
