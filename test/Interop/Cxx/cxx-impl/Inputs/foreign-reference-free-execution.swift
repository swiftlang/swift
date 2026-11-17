import ForeignReferenceFree

extension Derived {
  // void Derived::free() override;
  @cxx @implementation
  public override func free() {
    note("Derived::free")
    super.free()
  }
}

// void inspect(Object *_Nonnull object);
@cxx @implementation
public func inspect(_ object: Object) {
  note("inspect")
  noteRefs(object.refs)
}
