open class A {
  public init() { }

  @objc(a1) open func f1() { }
  @objc(initWithInt:) open func f2(_ int: Int) { }
  @objc(setFoo:) open func f3(_ i: Int) { }
  @objc(objectAtIndexedSubscript:) open func f4(_ i: Int) { }
}
