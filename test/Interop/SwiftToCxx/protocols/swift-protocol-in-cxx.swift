// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Protocols -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/protocols.h -cxx-interoperability-mode=upcoming-swift
// RUN: %FileCheck %s < %t/protocols.h

// RUN: %check-interop-cxx-header-in-clang(%t/protocols.h -DSWIFT_CXX_INTEROP_UPCOMING_SWIFT)

// REQUIRES: objc_interop

// expected-no-diagnostics

public protocol Drawable {
    func draw() -> Int
}

public protocol Resizable {
    func resize(to factor: Int) -> Bool
}

public protocol Taggable: Sendable {}

@_marker public protocol Priority {}

public protocol Stylable: Drawable {
    func style() -> Bool
}

// CHECK-LABEL: namespace Protocols

// --- Non-marker protocol: Drawable ---

// CHECK-LABEL: class _impl_Drawable;

// CHECK:       class
// CHECK-SAME:  Drawable final : public swift::_impl::SwiftExistentialType {
// CHECK-NEXT:  public:
// CHECK:         swift::Int draw() const {
// CHECK-NEXT:      // Type-only witness signature (never instantiated).
// CHECK-NEXT:      struct _w { _w() = delete; static SWIFT_CALL swift::Int call(void *_Nonnull, const void *_Nonnull, SWIFT_CONTEXT void *_Nonnull); };
// CHECK-NEXT:      return _loadWitness<1, {{[0-9]+}}, decltype(&_w::call)>(_witnessTable)(_type, _witnessTable, _projectValue());
// CHECK-NEXT:    }
// CHECK:       private:
// CHECK:         Drawable() noexcept : SwiftExistentialType(uninit_t{}) {}
// CHECK:         const void *_Nonnull _witnessTable;
// CHECK:         friend class _impl::_impl_Drawable;
// CHECK:       };

// CHECK:       class _impl_Drawable {
// CHECK-NEXT:  public:
// CHECK-NEXT:  };

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
// CHECK-NEXT:  };

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
// CHECK:       private:
// CHECK:         Stylable() noexcept : SwiftExistentialType(uninit_t{}) {}
// CHECK:         const void *_Nonnull _witnessTable;
// CHECK:         friend class _impl::_impl_Stylable;
// CHECK:       };

// CHECK:       class _impl_Stylable {
// CHECK-NEXT:  public:
// CHECK-NEXT:  };

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
// CHECK-NEXT:  };
