namespace Space {

template <class...> struct Ship;
template <class T, class... Args> struct Ship<T(Args...)> {};

using Orbiter = Ship<void(bool)>;

} // namespace Space
