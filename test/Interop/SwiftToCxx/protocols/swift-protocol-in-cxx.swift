// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Protocols -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/protocols.h -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature CxxExistentialInterop
// RUN: %FileCheck %s < %t/protocols.h

// RUN: %check-interop-cxx-header-in-clang(%t/protocols.h -DSWIFT_CXX_INTEROP_UPCOMING_SWIFT -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// REQUIRES: swift_feature_CxxExistentialInterop

// expected-no-diagnostics

public protocol Container<Element> {
    associatedtype Element
    func count() -> Int
}

public protocol Drawable {
    func draw() -> Int
}

public protocol Resizable {
    func resize(to factor: Int) -> Bool
}

public protocol Taggable: Sendable {}

@_marker public protocol Priority {}

// Class-bound protocol.
public protocol Renderable: AnyObject {
    func render() -> Int
}

public class Canvas: Renderable {
    var width: Int
    public init(width: Int) { self.width = width }
    public func render() -> Int { return width }
}

public protocol Stylable: Drawable {
    func style() -> Bool
}

// Non-copyable protocol.
public protocol Resource: ~Copyable {
    func use() -> Int
}

// Concrete type conforming to Drawable (for boxing constructor test).
public struct Circle: Drawable {
    var radius: Int
    public init(radius: Int) { self.radius = radius }
    public func draw() -> Int { return radius * radius }
}

// CHECK-LABEL: namespace Protocols

// --- Protocol with primary associated type: Container<Element> ---

// CHECK-LABEL: class _impl_Container;

// Tag struct for Container.
// CHECK:       struct ContainerTag {
// CHECK-NEXT:    using WitnessTable = const void *_Nonnull;
// CHECK-NEXT:  };

// CHECK:       template <typename Element = swift::Any>
// CHECK-NEXT:  class
// CHECK-SAME:  Container final : public swift::_impl::SwiftExistentialType<_impl::ContainerTag> {
// CHECK-NEXT:  public:
// CHECK:         swift::Int count() const {
// CHECK:       private:
// CHECK:         Container() noexcept : SwiftExistentialType(typename SwiftExistentialType::uninit_t{}) {}
// CHECK:         friend class _impl::_impl_Container;
// CHECK:       };

// CHECK:       class _impl_Container {
// CHECK-NEXT:  public:
// CHECK:         template <typename Element>
// CHECK-NEXT:    static {{.*}} Container<Element> _fromExistential(void *_Nonnull typeMetadata, void *_Nonnull projectedValue, const void *_Nonnull wt) {
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

// Tag struct for Drawable.
// CHECK:       struct DrawableTag {
// CHECK-NEXT:    using WitnessTable = const void *_Nonnull;
// CHECK-NEXT:  };

// Forward declaration of conforming type for boxing constructor.
// CHECK: class Circle;

// CHECK:       class
// CHECK-SAME:  Drawable final : public swift::_impl::SwiftExistentialType<_impl::DrawableTag> {
// CHECK-NEXT:  public:
// CHECK:         swift::Int draw() const {
// CHECK-NEXT:      // Type-only witness signature (never instantiated).
// CHECK-NEXT:      struct _w { _w() = delete; static SWIFT_CALL swift::Int call(void *_Nonnull, const void *_Nonnull, SWIFT_CONTEXT void *_Nonnull); };
// CHECK-NEXT:      return _loadWitness<1, {{[0-9]+}}, decltype(&_w::call)>(_witnessTables[0])(_type, _witnessTables[0], _projectValue());
// CHECK-NEXT:    }
// CHECK:         Drawable(const Circle &value) noexcept;
// CHECK:       private:
// CHECK:         Drawable() noexcept : SwiftExistentialType(typename SwiftExistentialType::uninit_t{}) {}
// CHECK:         friend class _impl::_impl_Drawable;
// CHECK:       };

// CHECK:       class _impl_Drawable {
// CHECK-NEXT:  public:
// CHECK:         static {{.*}} Drawable _fromExistential(void *_Nonnull typeMetadata, void *_Nonnull projectedValue, const void *_Nonnull wt) {
// CHECK-NEXT:      Drawable result;
// CHECK-NEXT:      result._type = typeMetadata;
// CHECK-NEXT:      result._initializeWithValue(projectedValue);
// CHECK-NEXT:      result._witnessTables[0] = wt;
// CHECK-NEXT:      return result;
// CHECK-NEXT:    }
// CHECK:         static {{.*}} const char * _Nonnull getOpaquePointer(const Drawable &object)
// CHECK:         static {{.*}} char * _Nonnull getOpaquePointer(Drawable &object)
// CHECK:         static {{.*}} Drawable returnNewValue(T callable) {
// CHECK:       };

// --- Marker protocol: Priority (SwiftExistentialType with MarkerTag) ---

// CHECK-LABEL: class _impl_Priority;

// CHECK:       struct PriorityTag {
// CHECK-NEXT:    static constexpr bool IsMarker = true;
// CHECK-NEXT:  };

// CHECK:       class
// CHECK-SAME:  Priority final : public swift::_impl::SwiftExistentialType<_impl::PriorityTag> {
// CHECK-NEXT:  public:
// CHECK-NEXT:  private:
// CHECK:         Priority() noexcept : SwiftExistentialType(typename SwiftExistentialType::uninit_t{}) {}
// CHECK:         friend class _impl::_impl_Priority;
// CHECK:       };

// CHECK:       class _impl_Priority {
// CHECK-NEXT:  public:
// CHECK-NEXT:  };

// --- Class-bound protocol: Renderable (SwiftClassExistentialType base) ---

// CHECK-LABEL: class _impl_Renderable;
// CHECK-NEXT: SWIFT_EXTERN const char $s9Protocols6CanvasCAA10RenderableAAWP[];

// CHECK:       struct RenderableTag {
// CHECK-NEXT:    using WitnessTable = const void *_Nonnull;
// CHECK-NEXT:  };

// CHECK: class Canvas;

// CHECK:       class
// CHECK-SAME:  Renderable final : public swift::_impl::SwiftClassExistentialType<_impl::RenderableTag> {
// CHECK-NEXT:  public:
// CHECK:         swift::Int render() const {
// CHECK-NEXT:      // Type-only witness signature (never instantiated).
// CHECK-NEXT:      struct _w { _w() = delete; static SWIFT_CALL swift::Int call(void *_Nonnull, const void *_Nonnull, SWIFT_CONTEXT void *_Nonnull); };
// CHECK-NEXT:      return _loadWitness<1, {{[0-9]+}}, decltype(&_w::call)>(_witnessTables[0])(_getType(), _witnessTables[0], _projectValue());
// CHECK-NEXT:    }
// CHECK:         Renderable(const Canvas &value) noexcept;
// CHECK:       private:
// CHECK:         Renderable() noexcept : SwiftClassExistentialType(typename SwiftClassExistentialType::uninit_t{}) {}
// CHECK:         friend class _impl::_impl_Renderable;
// CHECK:       };

// CHECK:       class _impl_Renderable {
// CHECK-NEXT:  public:
// CHECK:         static {{.*}} Renderable _fromExistential(void *_Nullable classPtr, const void *_Nonnull wt) {
// CHECK:         static {{.*}} const char * _Nonnull getOpaquePointer(const Renderable &object)
// CHECK:         static {{.*}} Renderable returnNewValue(T callable) {
// CHECK:       };

// --- Non-marker protocol: Resizable ---

// CHECK-LABEL: class _impl_Resizable;

// CHECK:       struct ResizableTag {
// CHECK-NEXT:    using WitnessTable = const void *_Nonnull;
// CHECK-NEXT:  };

// CHECK:       class
// CHECK-SAME:  Resizable final : public swift::_impl::SwiftExistentialType<_impl::ResizableTag> {
// CHECK-NEXT:  public:
// CHECK:         bool resize(swift::Int factor) const {
// CHECK-NEXT:      // Type-only witness signature (never instantiated).
// CHECK-NEXT:      struct _w { _w() = delete; static SWIFT_CALL bool call(swift::Int, void *_Nonnull, const void *_Nonnull, SWIFT_CONTEXT void *_Nonnull); };
// CHECK-NEXT:      return _loadWitness<1, {{[0-9]+}}, decltype(&_w::call)>(_witnessTables[0])(factor, _type, _witnessTables[0], _projectValue());
// CHECK-NEXT:    }
// CHECK:       private:
// CHECK:         Resizable() noexcept : SwiftExistentialType(typename SwiftExistentialType::uninit_t{}) {}
// CHECK:         friend class _impl::_impl_Resizable;
// CHECK:       };

// CHECK:       class _impl_Resizable {
// CHECK-NEXT:  public:
// CHECK:         static {{.*}} Resizable _fromExistential(void *_Nonnull typeMetadata, void *_Nonnull projectedValue, const void *_Nonnull wt) {
// CHECK:         static {{.*}} const char * _Nonnull getOpaquePointer(const Resizable &object)
// CHECK:         static {{.*}} Resizable returnNewValue(T callable) {
// CHECK:       };

// --- Non-copyable protocol: Resource (NonCopyable inverse tag) ---

// CHECK-LABEL: class _impl_Resource;

// CHECK:       struct ResourceTag {
// CHECK-NEXT:    using WitnessTable = const void *_Nonnull;
// CHECK-NEXT:  };

// CHECK:       class
// CHECK-SAME:  Resource final : public swift::_impl::SwiftExistentialType<_impl::ResourceTag, swift::_impl::NonCopyable> {
// CHECK-NEXT:  public:
// CHECK:         swift::Int use() const {
// CHECK:       private:
// CHECK:         Resource() noexcept : SwiftExistentialType(typename SwiftExistentialType::uninit_t{}) {}
// CHECK:         friend class _impl::_impl_Resource;
// CHECK:       };

// CHECK:       class _impl_Resource {
// CHECK-NEXT:  public:
// CHECK:         static {{.*}} Resource _fromExistential(void *_Nonnull typeMetadata, void *_Nonnull projectedValue, const void *_Nonnull wt) {
// CHECK:         static {{.*}} const char * _Nonnull getOpaquePointer(const Resource &object)
// CHECK:         static {{.*}} Resource returnNewValue(T callable) {
// CHECK:       };

// --- Non-marker protocol inheriting Drawable: Stylable ---
// Inherited draw() uses two-level dispatch through base WT.

// CHECK-LABEL: class _impl_Stylable;

// CHECK:       struct StylableTag {
// CHECK-NEXT:    using WitnessTable = const void *_Nonnull;
// CHECK-NEXT:  };

// CHECK:       class
// CHECK-SAME:  Stylable final : public swift::_impl::SwiftExistentialType<_impl::StylableTag> {
// CHECK-NEXT:  public:
// CHECK:         swift::Int draw() const {
// CHECK-NEXT:      // Type-only witness signature (never instantiated).
// CHECK-NEXT:      struct _w { _w() = delete; static SWIFT_CALL swift::Int call(void *_Nonnull, const void *_Nonnull, SWIFT_CONTEXT void *_Nonnull); };
// CHECK-NEXT:      auto *_bwt0 = reinterpret_cast<const void *const *>(_witnessTables[0])[1];
// CHECK-NEXT:      return _loadWitness<1, {{[0-9]+}}, decltype(&_w::call)>(_bwt0)(_type, _bwt0, _projectValue());
// CHECK-NEXT:    }
// CHECK:         bool style() const {
// CHECK-NEXT:      // Type-only witness signature (never instantiated).
// CHECK-NEXT:      struct _w { _w() = delete; static SWIFT_CALL bool call(void *_Nonnull, const void *_Nonnull, SWIFT_CONTEXT void *_Nonnull); };
// CHECK-NEXT:      return _loadWitness<2, {{[0-9]+}}, decltype(&_w::call)>(_witnessTables[0])(_type, _witnessTables[0], _projectValue());
// CHECK-NEXT:    }
// CHECK:         Drawable asDrawable() const {
// CHECK-NEXT:      auto *baseWT = reinterpret_cast<const void *const *>(_witnessTables[0])[1];
// CHECK-NEXT:      return _impl::_impl_Drawable::_fromExistential(_type, _projectValue(), baseWT);
// CHECK-NEXT:    }
// CHECK:       private:
// CHECK:         Stylable() noexcept : SwiftExistentialType(typename SwiftExistentialType::uninit_t{}) {}
// CHECK:         friend class _impl::_impl_Stylable;
// CHECK:       };

// CHECK:       class _impl_Stylable {
// CHECK-NEXT:  public:
// CHECK:         static {{.*}} Stylable _fromExistential(void *_Nonnull typeMetadata, void *_Nonnull projectedValue, const void *_Nonnull wt) {
// CHECK:         static {{.*}} const char * _Nonnull getOpaquePointer(const Stylable &object)
// CHECK:         static {{.*}} Stylable returnNewValue(T callable) {
// CHECK:       };

// --- Non-marker protocol inheriting Sendable: Taggable ---

// CHECK-LABEL: class _impl_Taggable;

// CHECK:       struct TaggableTag {
// CHECK-NEXT:    using WitnessTable = const void *_Nonnull;
// CHECK-NEXT:  };

// CHECK:       class
// CHECK-SAME:  Taggable final : public swift::_impl::SwiftExistentialType<_impl::TaggableTag> {
// CHECK-NEXT:  public:
// CHECK-NEXT:  private:
// CHECK:         Taggable() noexcept : SwiftExistentialType(typename SwiftExistentialType::uninit_t{}) {}
// CHECK:         friend class _impl::_impl_Taggable;
// CHECK:       };

// CHECK:       class _impl_Taggable {
// CHECK-NEXT:  public:
// CHECK:         static {{.*}} Taggable _fromExistential(void *_Nonnull typeMetadata, void *_Nonnull projectedValue, const void *_Nonnull wt) {
// CHECK:         static {{.*}} const char * _Nonnull getOpaquePointer(const Taggable &object)
// CHECK:         static {{.*}} Taggable returnNewValue(T callable) {
// CHECK:       };

// --- Out-of-line boxing constructor definitions (emitted after all types) ---

// CHECK-LABEL: Drawable::Drawable(const Circle &value) noexcept
// CHECK-NEXT:    : SwiftExistentialType(typename SwiftExistentialType::uninit_t{}) {
// CHECK-NEXT:    _type = swift::TypeMetadataTrait<Circle>::getTypeMetadata();
// CHECK-NEXT:    _initializeWithValue(_impl::_impl_Circle::getOpaquePointer(value));
// CHECK-NEXT:    _witnessTables[0] = reinterpret_cast<const void *>(_impl::$s9Protocols6CircleVAA8DrawableAAWP);
// CHECK-NEXT:  }

// CHECK-LABEL: Renderable::Renderable(const Canvas &value) noexcept
// CHECK-NEXT:    : SwiftClassExistentialType(typename SwiftClassExistentialType::uninit_t{}) {
// CHECK-NEXT:    _value = swift::_impl::_impl_RefCountedClass::getOpaquePointer(value);
// CHECK-NEXT:    swift::_impl::swift_retain(reinterpret_cast<void *_Nonnull>(_value));
// CHECK-NEXT:    _witnessTables[0] = reinterpret_cast<const void *>(_impl::$s9Protocols6CanvasCAA10RenderableAAWP);
// CHECK-NEXT:  }
