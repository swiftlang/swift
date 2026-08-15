protocol Animal {}

struct Horse: Animal {}

func someHorse(_ n: Int) -> Horse {
  print(n)
  return Horse()
}

func anyHorse(_ n: Int) -> any Animal {
  print(n)
  return Horse()
}

func f(x: some Animal, y: some Animal, z: some Animal) {}
f(x: anyHorse(1), y: someHorse(2), z: anyHorse(3))
