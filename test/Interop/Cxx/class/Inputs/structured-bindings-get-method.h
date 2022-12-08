#ifndef TEST_INTEROP_CXX_CLASS_STRUCTURED_BINDINGS_H
#define TEST_INTEROP_CXX_CLASS_STRUCTURED_BINDINGS_H

namespace std {

template<class X, class Y>
struct pear {
    pear(X x, Y y) {}

    X getX() const {
        return 0;
    }
    Y getY() const {
        return 42;
    }
};

template<class T>
struct tuple_size {
    constexpr static const int value = 2;
};

template<int n, class T>
struct tuple_element {
};

template<class X, class Y>
struct tuple_element<0, pear<X, Y>> {
    using type = X;
};

template<class X, class Y>
struct tuple_element<1, pear<X, Y>> {
    using type = Y;
};

template<int N, class X, class Y>
inline typename tuple_element<N, pear<X, Y>>::type get(const pear<X, Y> & value) {
    if constexpr (N == 0) {
        return value.getX();
    } else {
        return value.getY();
    }
}

} // namespace std

inline int testDestructure(int x) {
    auto val = std::pear<int, int>( x, x + 2 );
    auto [y,z] = val;
    return z;
}

#endif // TEST_INTEROP_CXX_CLASS_STRUCTURED_BINDINGS_H
