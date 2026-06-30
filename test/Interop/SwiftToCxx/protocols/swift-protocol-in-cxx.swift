// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Protocols -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/protocols.h -cxx-interoperability-mode=upcoming-swift
// RUN: %FileCheck %s < %t/protocols.h

// RUN: %check-interop-cxx-header-in-clang(%t/protocols.h -DSWIFT_CXX_INTEROP_UPCOMING_SWIFT -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: objc_interop

// expected-no-diagnostics

public protocol Drawable {
    func draw() -> Int
}

public protocol Container<Element> {
    associatedtype Element
    func count() -> Int
}

public protocol Resizable {
    func resize(to factor: Int) -> Bool
}

public protocol Taggable: Sendable {}

@_marker public protocol Priority {}

public protocol Stylable: Drawable {
    func style() -> Bool
}

// Concrete type conforming to Drawable (for boxing constructor test).
public struct Circle: Drawable {
    var radius: Int
    public init(radius: Int) { self.radius = radius }
    public func draw() -> Int { return radius * radius }
}

// Functions with existential params/returns (Phase 0.5).
public func drawTwice(_ d: any Drawable) -> Int {
    return d.draw() + d.draw()
}

public func bestDrawable(_ a: any Drawable, _ b: any Drawable) -> any Drawable {
    return a.draw() >= b.draw() ? a : b
}

public func countItems(_ c: any Container<Int>) -> Int {
    return c.count()
}

public func firstContainer(_ a: any Container<Int>, _ b: any Container<Int>) -> any Container<Int> {
    return a
}

public func nestedContainer(_ c: any Container<any Container<any Drawable>>) -> Int {
    return c.count()
}

// Class-bound protocol.
public protocol Renderable: AnyObject {
    func render() -> Int
}

public class Canvas: Renderable {
    var width: Int
    public init(width: Int) { self.width = width }
    public func render() -> Int { return width }
}

public func renderTwice(_ r: any Renderable) -> Int {
    return r.render() + r.render()
}

public func bestRenderable(_ a: any Renderable, _ b: any Renderable) -> any Renderable {
    return a.render() >= b.render() ? a : b
}

// --- @objc protocol: emitted as @protocol in ObjC section, no C++ wrapper ---

import Foundation

@objc public protocol Paintable {
    func paint() -> Int
}

public class Wall: NSObject, Paintable {
    var area: Int
    public init(area: Int) { self.area = area; super.init() }
    public func paint() -> Int { return area * 2 }
}

public func paintTwice(_ p: any Paintable) -> Int {
    return p.paint() + p.paint()
}

// The @objc protocol appears as @protocol in the ObjC section.
// CHECK-LABEL: @protocol Paintable
// CHECK-NEXT:  - (NSInteger)paint SWIFT_WARN_UNUSED_RESULT;
// CHECK-NEXT:  @end

// CHECK-LABEL: namespace Protocols

// --- Protocol with primary associated type: Container<Element> ---

// CHECK-LABEL: class _impl_Container;

// CHECK:       template <typename Element = swift::Any>
// CHECK-NEXT:  class
// CHECK-SAME:  Container final : public swift::_impl::SwiftExistentialType {
// CHECK-NEXT:  public:
// CHECK:         swift::Int count() const {
// CHECK:       private:
// CHECK:         Container() noexcept : SwiftExistentialType(uninit_t{}) {}
// CHECK:         const void *_Nonnull _witnessTable;
// CHECK:         friend class _impl::_impl_Container;
// CHECK:       };

// CHECK:       class _impl_Container {
// CHECK-NEXT:  public:
// CHECK:         template <typename Element>
// CHECK-NEXT:    static {{.*}} Container<Element> _fromExistential(const swift::_impl::SwiftExistentialType &src, const void *_Nonnull wt) {
// CHECK-NEXT:      Container<Element> result;
// CHECK:         template <typename Element>
// CHECK-NEXT:    static {{.*}} const char * _Nonnull getOpaquePointer(const Container<Element> &object)
// CHECK:         template <typename Element>
// CHECK-NEXT:    static {{.*}} char * _Nonnull getOpaquePointer(Container<Element> &object)
// CHECK:         static {{.*}} Container<Element> returnNewValue(T callable) {
// CHECK:       };

// --- Non-marker protocol: Drawable ---

// CHECK-LABEL: class _impl_Drawable;
// CHECK-NEXT: SWIFT_EXTERN const char $s9Protocols6CircleVAA8DrawableAAWP[];

// Forward declaration of conforming type for boxing constructor.
// CHECK: class Circle;

// CHECK:       class
// CHECK-SAME:  Drawable final : public swift::_impl::SwiftExistentialType {
// CHECK-NEXT:  public:
// CHECK:         swift::Int draw() const {
// CHECK-NEXT:      // Type-only witness signature (never instantiated).
// CHECK-NEXT:      struct _w { _w() = delete; static SWIFT_CALL swift::Int call(void *_Nonnull, const void *_Nonnull, SWIFT_CONTEXT void *_Nonnull); };
// CHECK-NEXT:      return _loadWitness<1, {{[0-9]+}}, decltype(&_w::call)>(_witnessTable)(_type, _witnessTable, _projectValue());
// CHECK-NEXT:    }
// CHECK:         Drawable(const Circle &value) noexcept;
// CHECK:       private:
// CHECK:         Drawable() noexcept : SwiftExistentialType(uninit_t{}) {}
// CHECK:         const void *_Nonnull _witnessTable;
// CHECK:         friend class _impl::_impl_Drawable;
// CHECK:       };

// CHECK:       class _impl_Drawable {
// CHECK-NEXT:  public:
// CHECK:         static {{.*}} Drawable _fromExistential(const swift::_impl::SwiftExistentialType &src, const void *_Nonnull wt) {
// CHECK-NEXT:      Drawable result;
// CHECK-NEXT:      result._initializeWithCopy(src);
// CHECK-NEXT:      result._witnessTable = wt;
// CHECK-NEXT:      return result;
// CHECK-NEXT:    }
// CHECK:         static {{.*}} const char * _Nonnull getOpaquePointer(const Drawable &object)
// CHECK:         static {{.*}} char * _Nonnull getOpaquePointer(Drawable &object)
// CHECK:         static {{.*}} Drawable returnNewValue(T callable) {
// CHECK:       };

// --- Marker protocol: Priority (subclass of swift::Any) ---

// CHECK-LABEL: class _impl_Priority;

// CHECK:       class
// CHECK-SAME:  Priority final : public swift::Any {
// CHECK-NEXT:  public:
// CHECK-NEXT:  private:
// CHECK:         Priority() noexcept : Any() {}
// CHECK-NOT:     _witnessTable
// CHECK:         friend class _impl::_impl_Priority;
// CHECK:       };

// CHECK:       class _impl_Priority {
// CHECK-NEXT:  public:
// CHECK-NEXT:  };

// --- Class-bound protocol: Renderable (SwiftClassExistentialType base) ---

// CHECK-LABEL: class _impl_Renderable;
// CHECK-NEXT: SWIFT_EXTERN const char $s9Protocols6CanvasCAA10RenderableAAWP[];

// CHECK: class Canvas;

// CHECK:       class
// CHECK-SAME:  Renderable final : public swift::_impl::SwiftClassExistentialType {
// CHECK-NEXT:  public:
// CHECK:         swift::Int render() const {
// CHECK-NEXT:      // Type-only witness signature (never instantiated).
// CHECK-NEXT:      struct _w { _w() = delete; static SWIFT_CALL swift::Int call(void *_Nonnull, const void *_Nonnull, SWIFT_CONTEXT void *_Nonnull); };
// CHECK-NEXT:      return _loadWitness<1, {{[0-9]+}}, decltype(&_w::call)>(_witnessTable)(_getType(), _witnessTable, _projectValue());
// CHECK-NEXT:    }
// CHECK:         Renderable(const Canvas &value) noexcept;
// CHECK:       private:
// CHECK:         Renderable() noexcept : SwiftClassExistentialType(uninit_t{}) {}
// CHECK:         const void *_Nonnull _witnessTable;
// CHECK:         friend class _impl::_impl_Renderable;
// CHECK:       };

// CHECK:       class _impl_Renderable {
// CHECK-NEXT:  public:
// CHECK:         static {{.*}} Renderable _fromExistential(const swift::_impl::SwiftClassExistentialType &src, const void *_Nonnull wt) {
// CHECK:         static {{.*}} const char * _Nonnull getOpaquePointer(const Renderable &object)
// CHECK:         static {{.*}} Renderable returnNewValue(T callable) {
// CHECK:       };

// --- Non-marker protocol: Resizable ---

// CHECK-LABEL: class _impl_Resizable;

// CHECK:       class
// CHECK-SAME:  Resizable final : public swift::_impl::SwiftExistentialType {
// CHECK-NEXT:  public:
// CHECK:         bool resize(swift::Int factor) const {
// CHECK-NEXT:      // Type-only witness signature (never instantiated).
// CHECK-NEXT:      struct _w { _w() = delete; static SWIFT_CALL bool call(swift::Int, void *_Nonnull, const void *_Nonnull, SWIFT_CONTEXT void *_Nonnull); };
// CHECK-NEXT:      return _loadWitness<1, {{[0-9]+}}, decltype(&_w::call)>(_witnessTable)(factor, _type, _witnessTable, _projectValue());
// CHECK-NEXT:    }
// CHECK:       private:
// CHECK:         Resizable() noexcept : SwiftExistentialType(uninit_t{}) {}
// CHECK:         const void *_Nonnull _witnessTable;
// CHECK:         friend class _impl::_impl_Resizable;
// CHECK:       };

// CHECK:       class _impl_Resizable {
// CHECK-NEXT:  public:
// CHECK:         static {{.*}} Resizable _fromExistential(const swift::_impl::SwiftExistentialType &src, const void *_Nonnull wt) {
// CHECK:         static {{.*}} const char * _Nonnull getOpaquePointer(const Resizable &object)
// CHECK:         static {{.*}} Resizable returnNewValue(T callable) {
// CHECK:       };

// --- Non-marker protocol inheriting Drawable: Stylable ---
// Inherited draw() uses two-level dispatch through base WT.

// CHECK-LABEL: class _impl_Stylable;

// CHECK:       class
// CHECK-SAME:  Stylable final : public swift::_impl::SwiftExistentialType {
// CHECK-NEXT:  public:
// CHECK:         swift::Int draw() const {
// CHECK-NEXT:      // Type-only witness signature (never instantiated).
// CHECK-NEXT:      struct _w { _w() = delete; static SWIFT_CALL swift::Int call(void *_Nonnull, const void *_Nonnull, SWIFT_CONTEXT void *_Nonnull); };
// CHECK-NEXT:      auto *_bwt0 = reinterpret_cast<const void *const *>(_witnessTable)[1];
// CHECK-NEXT:      return _loadWitness<1, {{[0-9]+}}, decltype(&_w::call)>(_bwt0)(_type, _bwt0, _projectValue());
// CHECK-NEXT:    }
// CHECK:         bool style() const {
// CHECK-NEXT:      // Type-only witness signature (never instantiated).
// CHECK-NEXT:      struct _w { _w() = delete; static SWIFT_CALL bool call(void *_Nonnull, const void *_Nonnull, SWIFT_CONTEXT void *_Nonnull); };
// CHECK-NEXT:      return _loadWitness<2, {{[0-9]+}}, decltype(&_w::call)>(_witnessTable)(_type, _witnessTable, _projectValue());
// CHECK-NEXT:    }
// CHECK:         Drawable asDrawable() const {
// CHECK-NEXT:      auto *baseWT = reinterpret_cast<const void *const *>(_witnessTable)[1];
// CHECK-NEXT:      return _impl::_impl_Drawable::_fromExistential(*this, baseWT);
// CHECK-NEXT:    }
// CHECK:       private:
// CHECK:         Stylable() noexcept : SwiftExistentialType(uninit_t{}) {}
// CHECK:         const void *_Nonnull _witnessTable;
// CHECK:         friend class _impl::_impl_Stylable;
// CHECK:       };

// CHECK:       class _impl_Stylable {
// CHECK-NEXT:  public:
// CHECK:         static {{.*}} Stylable _fromExistential(const swift::_impl::SwiftExistentialType &src, const void *_Nonnull wt) {
// CHECK:         static {{.*}} const char * _Nonnull getOpaquePointer(const Stylable &object)
// CHECK:         static {{.*}} Stylable returnNewValue(T callable) {
// CHECK:       };

// --- Non-marker protocol inheriting Sendable: Taggable ---

// CHECK-LABEL: class _impl_Taggable;

// CHECK:       class
// CHECK-SAME:  Taggable final : public swift::_impl::SwiftExistentialType {
// CHECK-NEXT:  public:
// CHECK-NEXT:  private:
// CHECK:         Taggable() noexcept : SwiftExistentialType(uninit_t{}) {}
// CHECK:         const void *_Nonnull _witnessTable;
// CHECK:         friend class _impl::_impl_Taggable;
// CHECK:       };

// CHECK:       class _impl_Taggable {
// CHECK-NEXT:  public:
// CHECK:         static {{.*}} Taggable _fromExistential(const swift::_impl::SwiftExistentialType &src, const void *_Nonnull wt) {
// CHECK:         static {{.*}} const char * _Nonnull getOpaquePointer(const Taggable &object)
// CHECK:         static {{.*}} Taggable returnNewValue(T callable) {
// CHECK:       };

// --- Functions with existential params/returns (Phase 0.5) ---

// CHECK-LABEL: Drawable bestDrawable(const Drawable& a, const Drawable& b)
// CHECK:         return _impl::_impl_Drawable::returnNewValue([&](char * _Nonnull result)
// CHECK:           _impl::_impl_Drawable::getOpaquePointer(a)
// CHECK:           _impl::_impl_Drawable::getOpaquePointer(b)

// CHECK-LABEL: Renderable bestRenderable(const Renderable& a, const Renderable& b)
// CHECK:         return _impl::_impl_Renderable::returnNewValue([&](char * _Nonnull result)
// CHECK:           swift_interop_passDirect_Protocols_void_ptr_0_8_void_ptr_8_16(_impl::_impl_Renderable::getOpaquePointer(a))
// CHECK:           swift_interop_passDirect_Protocols_void_ptr_0_8_void_ptr_8_16(_impl::_impl_Renderable::getOpaquePointer(b))

// CHECK-LABEL: swift::Int countItems(const Container<swift::Int>& c)
// CHECK:         _impl::_impl_Container::getOpaquePointer(c)

// CHECK-LABEL: swift::Int drawTwice(const Drawable& d)
// CHECK:         _impl::_impl_Drawable::getOpaquePointer(d)

// CHECK-LABEL: Container<swift::Int> firstContainer(const Container<swift::Int>& a, const Container<swift::Int>& b)
// CHECK:         return _impl::_impl_Container::returnNewValue<swift::Int>([&](char * _Nonnull result)
// CHECK:           _impl::_impl_Container::getOpaquePointer(a)
// CHECK:           _impl::_impl_Container::getOpaquePointer(b)

// CHECK-LABEL: swift::Int nestedContainer(const Container<Container<Drawable>>& c)
// CHECK:         _impl::_impl_Container::getOpaquePointer(c)

// --- @objc protocol: no C++ existential wrapper, function uses id<Paintable> ---

// The function taking 'any Paintable' uses id<Paintable> in the thunk.
// CHECK-LABEL: paintTwice(id <Paintable> _Nonnull p)
// CHECK:         $s9Protocols10paintTwiceySiAA9Paintable_pF

// CHECK-LABEL: swift::Int renderTwice(const Renderable& r)
// CHECK:         swift_interop_passDirect_Protocols_void_ptr_0_8_void_ptr_8_16(_impl::_impl_Renderable::getOpaquePointer(r))

// --- Out-of-line boxing constructor definitions (emitted after all types) ---

// CHECK-LABEL: Drawable::Drawable(const Circle &value) noexcept
// CHECK-NEXT:    : SwiftExistentialType(uninit_t{}) {
// CHECK-NEXT:    _type = swift::TypeMetadataTrait<Circle>::getTypeMetadata();
// CHECK-NEXT:    _initializeWithValue(_impl::_impl_Circle::getOpaquePointer(value));
// CHECK-NEXT:    _witnessTable = reinterpret_cast<const void *>(_impl::$s9Protocols6CircleVAA8DrawableAAWP);
// CHECK-NEXT:  }

// CHECK-LABEL: Renderable::Renderable(const Canvas &value) noexcept
// CHECK-NEXT:    : SwiftClassExistentialType(uninit_t{}) {
// CHECK-NEXT:    _value = swift::_impl::_impl_RefCountedClass::getOpaquePointer(value);
// CHECK-NEXT:    swift::_impl::swift_retain(_value);
// CHECK-NEXT:    _witnessTable = reinterpret_cast<const void *>(_impl::$s9Protocols6CanvasCAA10RenderableAAWP);
// CHECK-NEXT:  }

// The @objc protocol must NOT get a SwiftExistentialType wrapper class
// anywhere in the generated header.
// CHECK-NOT: class{{.*}}Paintable{{.*}}SwiftExistentialType
// CHECK-NOT: class{{.*}}Paintable{{.*}}SwiftClassExistentialType
