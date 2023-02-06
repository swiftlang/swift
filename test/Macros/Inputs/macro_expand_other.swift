
struct DogObserver: Observer {
  typealias Subject = Dog
}

func observeDog() {
  let dog = Dog()
  dog.name = "George"
  dog.treat = Treat()
  dog.bark()
  dog.addObserver(DogObserver())
}
