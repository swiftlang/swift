// RUN: %target-swift-frontend %s \
// RUN: -emit-sil  -target %target-swift-5.1-abi-triple \
// RUN: -enable-experimental-feature Lifetimes \
// RUN: | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes

struct BufferView : ~Escapable {
  let ptr: UnsafeRawBufferPointer
  let c: Int
  @_lifetime(borrow ptr)
  init(_ ptr: UnsafeRawBufferPointer, _ c: Int) {
    self.ptr = ptr
    self.c = c
  }
  @_lifetime(borrow ptr)
  init(independent ptr: UnsafeRawBufferPointer, _ c: Int) {
    self.ptr = ptr
    self.c = c
  }
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence10BufferViewVyA2ChcfC : $@convention(method) (@guaranteed BufferView, @thin BufferView.Type) -> @lifetime(copy 0) @owned BufferView {
  @_lifetime(copy otherBV)
  init(_ otherBV: borrowing BufferView) {
    self.ptr = otherBV.ptr
    self.c = otherBV.c
  }
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence10BufferViewVyA2CcfC : $@convention(method) (@owned BufferView, @thin BufferView.Type) -> @lifetime(copy 0) @owned BufferView {
  @_lifetime(copy otherBV)
  init(_ otherBV: consuming BufferView) {
    self.ptr = otherBV.ptr
    self.c = otherBV.c
  }
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence10BufferViewVyACSW_SaySiGhtcfC : $@convention(method) (UnsafeRawBufferPointer, @guaranteed Array<Int>, @thin BufferView.Type) -> @lifetime(borrow 0) @owned BufferView {
  @_lifetime(borrow ptr)
  init(_ ptr: UnsafeRawBufferPointer, _ a: borrowing Array<Int>) {
    self.ptr = ptr
    self.c = a.count
  }
}

struct MutableBufferView : ~Escapable, ~Copyable {
  let ptr: UnsafeMutableRawBufferPointer
  let c: Int
  @_lifetime(borrow ptr)
  init(_ ptr: UnsafeMutableRawBufferPointer, _ c: Int) {
    self.ptr = ptr
    self.c = c
  }
}

func testBasic() {
  let a = [Int](repeating: 0, count: 4)
  a.withUnsafeBytes {
    let view = BufferView($0, a.count)
    let derivedView = derive(view)
    let newView = consumeAndCreate(derivedView)
    use(newView)
  }
}

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence6deriveyAA10BufferViewVADF : $@convention(thin) (@guaranteed BufferView) -> @lifetime(copy 0)  @owned BufferView {
@_lifetime(copy x)
func derive(_ x: borrowing BufferView) -> BufferView {
  let newBV = BufferView(x.ptr, x.c)
  return _overrideLifetime(newBV, copying: x)
}

@_lifetime(copy x)
func derive(_ unused: Int, _ x: borrowing BufferView) -> BufferView {
  let newBV = BufferView(independent: x.ptr, x.c)
  return _overrideLifetime(newBV, copying: x)
}

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence16consumeAndCreateyAA10BufferViewVADnF : $@convention(thin) (@owned BufferView) -> @lifetime(copy 0)  @owned BufferView {
@_lifetime(copy x)
func consumeAndCreate(_ x: consuming BufferView) -> BufferView {
  let bv = BufferView(independent: x.ptr, x.c)
  return _overrideLifetime(bv, copying: x)
}

func use(_ x: borrowing BufferView) {}

struct Wrapper : ~Escapable {
  var _view: BufferView
  var view: BufferView {
// CHECK: sil hidden @$s28implicit_lifetime_dependence7WrapperV4viewAA10BufferViewVvr : $@yield_once @convention(method) (@guaranteed Wrapper) -> @lifetime(copy 0) @yields @guaranteed BufferView {
    @_lifetime(copy self)
    _read {
      yield _view
    }
// CHECK: sil hidden @$s28implicit_lifetime_dependence7WrapperV4viewAA10BufferViewVvM : $@yield_once @convention(method) (@inout Wrapper) -> @lifetime(borrow 0) @yields @inout BufferView {
    @_lifetime(borrow self)
    _modify {
      yield &_view
    }
  }
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence7WrapperVyAcA10BufferViewVcfC : $@convention(method) (@owned BufferView, @thin Wrapper.Type) -> @lifetime(copy 0) @owned Wrapper {
  @_lifetime(copy view)
  init(_ view: consuming BufferView) {
    self._view = view
  }
// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence7WrapperV8getView1AA10BufferViewVyKF : $@convention(method) (@guaranteed Wrapper) -> @lifetime(copy 0)  (@owned BufferView, @error any Error) {
  @_lifetime(copy self)
  borrowing func getView1() throws -> BufferView {
    return _view
  }

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence7WrapperV8getView2AA10BufferViewVyYaKF : $@convention(method) @async (@owned Wrapper) -> @lifetime(copy 0)  (@owned BufferView, @error any Error) {
  @_lifetime(copy self)
  consuming func getView2() async throws -> BufferView {
    return _view
  }
}

struct Container1 : ~Copyable {
  let ptr: UnsafeRawBufferPointer
  let c: Int
// CHECK: sil hidden @$s28implicit_lifetime_dependence10Container1V4viewAA10BufferViewVvg : $@convention(method) (@guaranteed Container1) -> @lifetime(borrow 0) @owned BufferView {
  var view: BufferView {
    get {
      return BufferView(ptr, c)
    }
  }
}

struct Container2 : ~Copyable {
  let ptr: UnsafeMutableRawBufferPointer
  let c: Int
// CHECK: sil hidden @$s28implicit_lifetime_dependence10Container2V11mutableViewAA013MutableBufferF0Vvr : $@yield_once @convention(method) (@guaranteed Container2) -> @lifetime(borrow 0) @yields @guaranteed MutableBufferView {
  var mutableView: MutableBufferView {
    _read {
      yield MutableBufferView(ptr, c)
    }
  }
}


struct FakeRange<Bound> {
  public let lowerBound: Bound
  public let upperBound: Bound
}

struct GenericBufferView<Element> : ~Escapable {
  typealias Index = Int
  typealias Pointer = UnsafeRawPointer

  let baseAddress: Pointer
  let count: Int

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence17GenericBufferViewV11baseAddress5countACyxGSV_SitcfC : $@convention(method) <Element> (UnsafeRawPointer, Int, @thin GenericBufferView<Element>.Type) -> @lifetime(borrow 0) @owned GenericBufferView<Element> {
  @_lifetime(borrow baseAddress)
  init<Storage>(baseAddress: Pointer,
                count: Int,
                dependsOn: borrowing Storage) {
    self = GenericBufferView<Element>(baseAddress: baseAddress,
                                      count: count)
  }
  // unsafe private API
  @_lifetime(borrow baseAddress)
  init(baseAddress: Pointer, count: Int) {
    precondition(count >= 0, "Count must not be negative")
    self.baseAddress = baseAddress
    self.count = count
  }
  subscript(position: Pointer) -> Element {
    get {
      if _isPOD(Element.self) {
        return position.loadUnaligned(as: Element.self)
      }
      else {
        return position.load(as: Element.self)
      }
    }
  }
// CHECK: sil hidden @$s28implicit_lifetime_dependence17GenericBufferViewVyACyxGAA9FakeRangeVySVGcig : $@convention(method) <Element> (FakeRange<UnsafeRawPointer>, @guaranteed GenericBufferView<Element>) -> @lifetime(copy 1) @owned GenericBufferView<Element> {
  subscript(bounds: FakeRange<Pointer>) -> Self {
    @_lifetime(copy self)
    get {
      let pointer = UnsafeRawPointer(bounds.lowerBound)
      let result = GenericBufferView(
        baseAddress: pointer,
        count: bounds.upperBound.distance(to:bounds.lowerBound) / MemoryLayout<Element>.stride
      )
      // assuming that bounds is within self
      return _overrideLifetime(result, copying: self)
    }
  }
}

// CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence23tupleLifetimeDependenceyAA10BufferViewV_ADtADF : $@convention(thin) (@guaranteed BufferView) -> @lifetime(copy 0)  (@owned BufferView, @owned BufferView) {
@_lifetime(copy x)
func tupleLifetimeDependence(_ x: borrowing BufferView) -> (BufferView, BufferView) {
  let newX1 = BufferView(x.ptr, x.c)
  let newX2 = BufferView(x.ptr, x.c)
  return (_overrideLifetime(newX1, copying: x), _overrideLifetime(newX2, copying: x))
}

public struct OuterNE: ~Escapable {
  // A public property generates an implicit setter with an infered dependence on 'newValue'.
  //
  // [inner1.setter]
  // CHECK-LABEL: sil [transparent] @$s28implicit_lifetime_dependence7OuterNEV6inner1AC05InnerE0Vvs : $@convention(method) (@owned OuterNE.InnerNE, @lifetime(copy 0, copy 1) @inout OuterNE) -> () {
  public var inner1: InnerNE

  // Explicit setter with an infered dependence on 'newValue'.
  public var inner2: InnerNE {
    @_lifetime(copy self)
    get { inner1 }
    @_lifetime(self: copy newValue)
    set { inner1 = newValue }
  }

  public struct InnerNE: ~Escapable {
    @_lifetime(copy owner)
    init<Owner: ~Escapable & ~Copyable>(
      owner: borrowing Owner
    ) {}
  }

  @_lifetime(copy owner)
  init<Owner: ~Copyable & ~Escapable>(owner: borrowing Owner) {
    self.inner1 = InnerNE(owner: owner)
  }

  // CHECK-LABEL: sil hidden @$s28implicit_lifetime_dependence7OuterNEV8setInner5valueyAC0gE0V_tF : $@convention(method) (@guaranteed OuterNE.InnerNE, @lifetime(copy 0) @inout OuterNE) -> () {
  @_lifetime(self: copy value)
  mutating func setInner(value: InnerNE) {
    self.inner1 = value
  }
}
