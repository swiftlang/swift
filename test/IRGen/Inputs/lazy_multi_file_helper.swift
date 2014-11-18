struct LazyContainer {
  lazy var lazyVar = 42
}

@inline(never)
func useLazyContainer(container: LazyContainer) {}


class LazyContainerClass {
  lazy var lazyVar = 42
}
