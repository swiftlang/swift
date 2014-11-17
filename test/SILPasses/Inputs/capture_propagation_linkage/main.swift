
class MyClass {
}

func neverCalled() {
	genericCaller(createNil)
}

func createNil() -> MyClass? {
    return nil
}

@inline(never)
func genericCaller<B>(f: () -> B) -> B {
        return f()
}

if createSome() != nil {
  println("test ok")
} else {
  println("test failed")
}

