func doStuff<E: Error>(_ errors: [E]) {
  for error in errors {
    print(error._domain)
  }
}
