template <int N, int M>
inline const int template_gcd = template_gcd<M, N % M>;

template <int N>
inline const int template_gcd<N, 0> = N;
