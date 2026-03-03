#ifndef CIRCULAR_INHERITANCE_H
#define CIRCULAR_INHERITANCE_H

struct DinoEgg {
  void dinoEgg(void) const {}
};

template <typename Chicken>
struct Egg;

template <>
struct Egg<void> : DinoEgg {
  Egg() {}
  void voidEgg(void) const {}
};

template <typename Chicken>
struct Egg : Egg<void> {
  Egg(Chicken _chicken) {}
  Chicken chickenEgg(Chicken c) const { return c; }
};

typedef Egg<void> VoidEgg;
typedef Egg<bool> BoolEgg;
typedef Egg<Egg<void>> EggEgg;

struct NewEgg : Egg<int> {
  NewEgg() : Egg<int>(555) {}
  void newEgg() const {}
};

#endif // !CIRCULAR_INHERITANCE_H
