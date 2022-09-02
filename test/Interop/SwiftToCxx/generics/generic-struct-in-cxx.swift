// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Generics -clang-header-expose-public-decls -emit-clang-header-path %t/generics.h
// RUN: %FileCheck %s < %t/generics.h
// RUN: %check-generic-interop-cxx-header-in-clang(%t/generics.h -Wno-reserved-identifier)

// Check that an instantiation compiles too.
// RUN: echo "constexpr int x = sizeof(Generics::GenericPair<int, int>);" >> %t/generics.h
// RUN: %check-generic-interop-cxx-header-in-clang(%t/generics.h -Wno-reserved-identifier)

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -enable-library-evolution -typecheck -module-name Generics -clang-header-expose-public-decls -emit-clang-header-path %t/generics.h
// RUN: %FileCheck %s < %t/generics.h
// RUN: %check-generic-interop-cxx-header-in-clang(%t/generics.h -Wno-reserved-identifier)

// FIXME: remove the need for -Wno-reserved-identifier

@frozen
public struct PairOfUInt64 {
    public let x: UInt64
    public let y: UInt64

    public init(_ x: UInt64,
                _ y: UInt64) {
        self.x = x
        self.y = y
    }
}

@frozen
public struct GenericPair<T, T2> {
    var x: T
    public var y: T2

    init(x: T, y: T2) {
        self.x = x
        self.y = y
    }

    public init(_ x: T, _ i: Int, _ y: T2) {
        self.x = x
        self.y = y
        print("GenericPair<T, T2>::init::\(x),\(y),\(i);")
    }

    public func method() {
        let copyOfSelf = self
        print("GenericPair<T, T2>::testme::\(x),\(copyOfSelf.y);")
    }

    public mutating func mutatingMethod(_ other: GenericPair<T2, T>) {
        x = other.y
        y = other.x
    }

    public func genericMethod<T>(_ x: T, _ y: T2) -> T {
        print("GenericPair<T, T2>::genericMethod<T>::\(x),\(y);")
        return x
    }

    public var computedProp: Int {
        return 42
    }

    public var computedVar: T {
        get {
            print("GenericPair<T, T2>::computeVar::get")
            return x
        } set {
            print("GenericPair<T, T2>::computeVar::set")
            x = newValue
        }
    }
}

public func makeGenericPair<T, T1>(_ x: T, _ y: T1) -> GenericPair<T, T1> {
    return GenericPair<T, T1>(x: x, y: y);
}

public func makeConcretePair(_ x: UInt32, _ y: UInt32) -> GenericPair<UInt32, UInt32> {
    return GenericPair<UInt32, UInt32>(x: x, y: y)
}

public func takeGenericPair<T, T1>(_ x: GenericPair<T, T1>) {
    print(x)
}

public func takeConcretePair(_ x: GenericPair<UInt32, UInt32>) {
    print("CONCRETE pair of UInt32: ", x.x, x.y, ";")
}

public func passThroughGenericPair<T1, T>(_ x: GenericPair<T1, T>, _ y: T)  -> GenericPair<T1, T> {
    return GenericPair<T1, T>(x: x.x, y: y)
}

public typealias ConcreteUint32Pair = GenericPair<UInt32, UInt32>

public func passThroughConcretePair(_ x: ConcreteUint32Pair, y: UInt32) -> ConcreteUint32Pair {
    return ConcreteUint32Pair(x: x.x, y: y)
}

public func inoutGenericPair<T1, T>(_ x: inout GenericPair<T1, T>, _ y: T1) {
    x.x = y
}

public func inoutConcretePair(_ x: UInt32, _ y: inout GenericPair<UInt32, UInt32>) {
    y.x = x
}

// CHECK: SWIFT_EXTERN void $s8Generics11GenericPairV1yq_vg(SWIFT_INDIRECT_RESULT void * _Nonnull, void * _Nonnull , SWIFT_CONTEXT const void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // _
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics11GenericPairV1yq_vs(const void * _Nonnull value, void * _Nonnull , SWIFT_CONTEXT void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // _
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics11GenericPairVyACyxq_Gx_Siq_tcfC(SWIFT_INDIRECT_RESULT void * _Nonnull, const void * _Nonnull x, ptrdiff_t i, const void * _Nonnull y, void * _Nonnull , void * _Nonnull ) SWIFT_NOEXCEPT SWIFT_CALL; // init(_:_:_:)
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics11GenericPairV6methodyyF(void * _Nonnull , SWIFT_CONTEXT const void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // method()
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics11GenericPairV14mutatingMethodyyACyq_xGF(const void * _Nonnull other, void * _Nonnull , SWIFT_CONTEXT void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // mutatingMethod(_:)
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics11GenericPairV13genericMethodyqd__qd___q_tlF(SWIFT_INDIRECT_RESULT void * _Nonnull, const void * _Nonnull x, const void * _Nonnull y, void * _Nonnull , void * _Nonnull , SWIFT_CONTEXT const void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // genericMethod(_:_:)
// CHECK-NEXT: SWIFT_EXTERN ptrdiff_t $s8Generics11GenericPairV12computedPropSivg(void * _Nonnull , SWIFT_CONTEXT const void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // _
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics11GenericPairV11computedVarxvg(SWIFT_INDIRECT_RESULT void * _Nonnull, void * _Nonnull , SWIFT_CONTEXT const void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // _
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics11GenericPairV11computedVarxvs(const void * _Nonnull newValue, void * _Nonnull , SWIFT_CONTEXT void * _Nonnull _self) SWIFT_NOEXCEPT SWIFT_CALL; // _

// CHECK: SWIFT_EXTERN void $s8Generics17inoutConcretePairyys6UInt32V_AA07GenericD0VyA2DGztF(uint32_t x, char * _Nonnull y) SWIFT_NOEXCEPT SWIFT_CALL; // inoutConcretePair(_:_:)
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics16inoutGenericPairyyAA0cD0Vyxq_Gz_xtr0_lF(void * _Nonnull x, const void * _Nonnull y, void * _Nonnull , void * _Nonnull ) SWIFT_NOEXCEPT SWIFT_CALL; // inoutGenericPair(_:_:)
// CHECK-NEXT: // Stub struct to be used to pass/return values to/from Swift functions.
// CHECK-NEXT: struct swift_interop_stub_Generics_GenericPair_s6UInt32V_s6UInt32V {
// CHECK-NEXT:   uint64_t _1;
// CHECK-NEXT: };
// CHECK-EMPTY:
// CHECK-NEXT: static inline void swift_interop_returnDirect_Generics_GenericPair_s6UInt32V_s6UInt32V(char * _Nonnull result, struct swift_interop_stub_Generics_GenericPair_s6UInt32V_s6UInt32V value) __attribute__((always_inline)) {
// CHECK-NEXT:   memcpy(result + 0, &value._1, 8);
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: static inline struct swift_interop_stub_Generics_GenericPair_s6UInt32V_s6UInt32V swift_interop_passDirect_Generics_GenericPair_s6UInt32V_s6UInt32V(const char * _Nonnull value) __attribute__((always_inline)) {
// CHECK-NEXT:   struct swift_interop_stub_Generics_GenericPair_s6UInt32V_s6UInt32V result;
// CHECK-NEXT:   memcpy(&result._1, value + 0, 8);
// CHECK-NEXT:   return result;
// CHECK-NEXT: }
// CHECK-EMPTY:
// CHECK-NEXT: SWIFT_EXTERN struct swift_interop_stub_Generics_GenericPair_s6UInt32V_s6UInt32V $s8Generics16makeConcretePairyAA07GenericD0Vys6UInt32VAFGAF_AFtF(uint32_t x, uint32_t y) SWIFT_NOEXCEPT SWIFT_CALL; // makeConcretePair(_:_:)
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics15makeGenericPairyAA0cD0Vyxq_Gx_q_tr0_lF(SWIFT_INDIRECT_RESULT void * _Nonnull, const void * _Nonnull x, const void * _Nonnull y, void * _Nonnull , void * _Nonnull ) SWIFT_NOEXCEPT SWIFT_CALL; // makeGenericPair(_:_:)
// CHECK-NEXT: SWIFT_EXTERN struct swift_interop_stub_Generics_GenericPair_s6UInt32V_s6UInt32V $s8Generics23passThroughConcretePair_1yAA07GenericE0Vys6UInt32VAGGAH_AGtF(struct swift_interop_stub_Generics_GenericPair_s6UInt32V_s6UInt32V x, uint32_t y) SWIFT_NOEXCEPT SWIFT_CALL; // passThroughConcretePair(_:y:)
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics22passThroughGenericPairyAA0dE0Vyxq_GAE_q_tr0_lF(SWIFT_INDIRECT_RESULT void * _Nonnull, const void * _Nonnull x, const void * _Nonnull y, void * _Nonnull , void * _Nonnull ) SWIFT_NOEXCEPT SWIFT_CALL; // passThroughGenericPair(_:_:)
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics16takeConcretePairyyAA07GenericD0Vys6UInt32VAFGF(struct swift_interop_stub_Generics_GenericPair_s6UInt32V_s6UInt32V x) SWIFT_NOEXCEPT SWIFT_CALL; // takeConcretePair(_:)
// CHECK-NEXT: SWIFT_EXTERN void $s8Generics15takeGenericPairyyAA0cD0Vyxq_Gr0_lF(const void * _Nonnull x, void * _Nonnull , void * _Nonnull ) SWIFT_NOEXCEPT SWIFT_CALL; // takeGenericPair(_:)

// CHECK: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: class _impl_GenericPair;
// CHECK-EMPTY:
// CHECK-NEXT: static_assert(2 <= 3, "unsupported generic requirement list for metadata func");
// CHECK-NEXT: // Type metadata accessor for GenericPair
// CHECK-NEXT: SWIFT_EXTERN swift::_impl::MetadataResponseTy $s8Generics11GenericPairVMa(swift::_impl::MetadataRequestTy, void * _Nonnull, void * _Nonnull) SWIFT_NOEXCEPT SWIFT_CALL;

// CHECK: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: class GenericPair final {
// CHECK-NEXT: public:
// CHECK-NEXT:   inline ~GenericPair() {
// CHECK-NEXT:     auto metadata = _impl::$s8Generics11GenericPairVMa(0, swift::TypeMetadataTrait<T_0_0>::getTypeMetadata(), swift::TypeMetadataTrait<T_0_1>::getTypeMetadata());

// CHECK: swift::_impl::OpaqueStorage _storage;
// CHECK-NEXT: friend class _impl::_impl_GenericPair<T_0_0, T_0_1>;
// CHECK-NEXT: }

// CHECK: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: class _impl_GenericPair {
// CHECK-NEXT: public:
// CHECK-NEXT:   static inline char * _Nonnull getOpaquePointer(GenericPair<T_0_0, T_0_1> &object) { return object._getOpaquePointer(); }
// CHECK-NEXT:   static inline const char * _Nonnull getOpaquePointer(const GenericPair<T_0_0, T_0_1> &object) { return object._getOpaquePointer(); }
// CHECK-NEXT: template<class T>
// CHECK-NEXT: static inline GenericPair<T_0_0, T_0_1> returnNewValue(T callable) {
// CHECK-NEXT:   auto result = GenericPair<T_0_0, T_0_1>::_make();
// CHECK-NEXT:   callable(result._getOpaquePointer());
// CHECK-NEXT:   return result;
// CHECK-NEXT: }

// CHECK: namespace swift {
// CHECK-NEXT: #pragma clang diagnostic push
// CHECK-NEXT: #pragma clang diagnostic ignored "-Wc++17-extensions"
// CHECK-NEXT: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: struct TypeMetadataTrait<Generics::GenericPair<T_0_0, T_0_1>> {
// CHECK-NEXT:   static inline void * _Nonnull getTypeMetadata() {
// CHECK-NEXT:     return Generics::_impl::$s8Generics11GenericPairVMa(0, swift::TypeMetadataTrait<T_0_0>::getTypeMetadata(), swift::TypeMetadataTrait<T_0_1>::getTypeMetadata())._0;
// CHECK-NEXT:   }
// CHECK-NEXT: };
// CHECK-NEXT: namespace _impl{
// CHECK-NEXT: } // namespace
// CHECK-NEXT: #pragma clang diagnostic pop
// CHECK-NEXT: } // namespace swift

// CHECK: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: class GenericPair;
// CHECK-EMPTY:
// CHECK-NEXT: inline void inoutConcretePair(uint32_t x, GenericPair<uint32_t, uint32_t>& y) noexcept {
// CHECK-NEXT:   return _impl::$s8Generics17inoutConcretePairyys6UInt32V_AA07GenericD0VyA2DGztF(x, _impl::_impl_GenericPair<uint32_t, uint32_t>::getOpaquePointer(y));
// CHECK-NEXT: }

// CHECK: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: inline void inoutGenericPair(GenericPair<T_0_0, T_0_1>& x, const T_0_0& y) noexcept {
// CHECK-NEXT:   return _impl::$s8Generics16inoutGenericPairyyAA0cD0Vyxq_Gz_xtr0_lF(_impl::_impl_GenericPair<T_0_0, T_0_1>::getOpaquePointer(x), swift::_impl::getOpaquePointer(y), swift::TypeMetadataTrait<T_0_0>::getTypeMetadata(), swift::TypeMetadataTrait<T_0_1>::getTypeMetadata());
// CHECK-NEXT: }

// CHECK: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: inline GenericPair<T_0_0, T_0_1> makeGenericPair(const T_0_0& x, const T_0_1& y) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::_impl_GenericPair<T_0_0, T_0_1>::returnNewValue([&](void * _Nonnull result) {
// CHECK-NEXT:     _impl::$s8Generics15makeGenericPairyAA0cD0Vyxq_Gx_q_tr0_lF(result, swift::_impl::getOpaquePointer(x), swift::_impl::getOpaquePointer(y), swift::TypeMetadataTrait<T_0_0>::getTypeMetadata(), swift::TypeMetadataTrait<T_0_1>::getTypeMetadata());
// CHECK-NEXT:   });
// CHECK-NEXT: }

// CHECK: inline GenericPair<uint32_t, uint32_t> passThroughConcretePair(const GenericPair<uint32_t, uint32_t>& x, uint32_t y) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:  return _impl::_impl_GenericPair<uint32_t, uint32_t>::returnNewValue([&](char * _Nonnull result) {
// CHECK-NEXT:     _impl::swift_interop_returnDirect_Generics_GenericPair_s6UInt32V_s6UInt32V(result, _impl::$s8Generics23passThroughConcretePair_1yAA07GenericE0Vys6UInt32VAGGAH_AGtF(_impl::swift_interop_passDirect_Generics_GenericPair_s6UInt32V_s6UInt32V(_impl::_impl_GenericPair<uint32_t, uint32_t>::getOpaquePointer(x)), y));
// CHECK-NEXT:   });
// CHECK-NEXT: }

// CHECK: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: inline GenericPair<T_0_0, T_0_1> passThroughGenericPair(const GenericPair<T_0_0, T_0_1>& x, const T_0_1& y) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:   return _impl::_impl_GenericPair<T_0_0, T_0_1>::returnNewValue([&](void * _Nonnull result) {
// CHECK-NEXT:     _impl::$s8Generics22passThroughGenericPairyAA0dE0Vyxq_GAE_q_tr0_lF(result, _impl::_impl_GenericPair<T_0_0, T_0_1>::getOpaquePointer(x), swift::_impl::getOpaquePointer(y), swift::TypeMetadataTrait<T_0_0>::getTypeMetadata(), swift::TypeMetadataTrait<T_0_1>::getTypeMetadata());
// CHECK-NEXT:   });
// CHECK-NEXT: }

// CHECK: inline void takeConcretePair(const GenericPair<uint32_t, uint32_t>& x) noexcept {
// CHECK-NEXT:  return _impl::$s8Generics16takeConcretePairyyAA07GenericD0Vys6UInt32VAFGF(_impl::swift_interop_passDirect_Generics_GenericPair_s6UInt32V_s6UInt32V(_impl::_impl_GenericPair<uint32_t, uint32_t>::getOpaquePointer(x)));
// CHECK-NEXT: }

// CHECK: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: inline void takeGenericPair(const GenericPair<T_0_0, T_0_1>& x) noexcept {
// CHECK-NEXT:  return _impl::$s8Generics15takeGenericPairyyAA0cD0Vyxq_Gr0_lF(_impl::_impl_GenericPair<T_0_0, T_0_1>::getOpaquePointer(x), swift::TypeMetadataTrait<T_0_0>::getTypeMetadata(), swift::TypeMetadataTrait<T_0_1>::getTypeMetadata());
// CHECK-NEXT:}

// CHECK: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: inline T_0_1 GenericPair<T_0_0, T_0_1>::getY() const {
// CHECK-NEXT: if constexpr (std::is_base_of<::swift::_impl::RefCountedClass, T_0_1>::value) {
// CHECK-NEXT: void *returnValue;
// CHECK-NEXT: _impl::$s8Generics11GenericPairV1yq_vg(reinterpret_cast<void *>(&returnValue), swift::TypeMetadataTrait<GenericPair<T_0_0, T_0_1>>::getTypeMetadata(), _getOpaquePointer());
// CHECK-NEXT: return ::swift::_impl::implClassFor<T_0_1>::type::makeRetained(returnValue);
// CHECK-NEXT: } else if constexpr (::swift::_impl::isValueType<T_0_1>) {
// CHECK-NEXT: return ::swift::_impl::implClassFor<T_0_1>::type::returnNewValue([&](void * _Nonnull returnValue) {
// CHECK-NEXT: _impl::$s8Generics11GenericPairV1yq_vg(returnValue, swift::TypeMetadataTrait<GenericPair<T_0_0, T_0_1>>::getTypeMetadata(), _getOpaquePointer());
// CHECK-NEXT: });
// CHECK-NEXT: } else {
// CHECK-NEXT: T_0_1 returnValue;
// CHECK-NEXT: _impl::$s8Generics11GenericPairV1yq_vg(reinterpret_cast<void *>(&returnValue), swift::TypeMetadataTrait<GenericPair<T_0_0, T_0_1>>::getTypeMetadata(), _getOpaquePointer());
// CHECK-NEXT: return returnValue;
// CHECK-NEXT: }
// CHECK-NEXT: }
// CHECK-NEXT: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: inline void GenericPair<T_0_0, T_0_1>::setY(const T_0_1& value) {
// CHECK-NEXT: return _impl::$s8Generics11GenericPairV1yq_vs(swift::_impl::getOpaquePointer(value), swift::TypeMetadataTrait<GenericPair<T_0_0, T_0_1>>::getTypeMetadata(), _getOpaquePointer());
// CHECK-NEXT: }
// CHECK-NEXT: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: inline GenericPair<T_0_0, T_0_1> GenericPair<T_0_0, T_0_1>::init(const T_0_0& x, swift::Int i, const T_0_1& y) {
// CHECK-NEXT: return _impl::_impl_GenericPair<T_0_0, T_0_1>::returnNewValue([&](void * _Nonnull result) {
// CHECK-NEXT:   _impl::$s8Generics11GenericPairVyACyxq_Gx_Siq_tcfC(result, swift::_impl::getOpaquePointer(x), i, swift::_impl::getOpaquePointer(y), swift::TypeMetadataTrait<T_0_0>::getTypeMetadata(), swift::TypeMetadataTrait<T_0_1>::getTypeMetadata());
// CHECK-NEXT: });
// CHECK-NEXT: }
// CHECK-NEXT: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: inline void GenericPair<T_0_0, T_0_1>::method() const {
// CHECK-NEXT: return _impl::$s8Generics11GenericPairV6methodyyF(swift::TypeMetadataTrait<GenericPair<T_0_0, T_0_1>>::getTypeMetadata(), _getOpaquePointer());
// CHECK-NEXT: }
// CHECK-NEXT: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: inline void GenericPair<T_0_0, T_0_1>::mutatingMethod(const GenericPair<T_0_1, T_0_0>& other) {
// CHECK-NEXT: return _impl::$s8Generics11GenericPairV14mutatingMethodyyACyq_xGF(_impl::_impl_GenericPair<T_0_1, T_0_0>::getOpaquePointer(other), swift::TypeMetadataTrait<GenericPair<T_0_0, T_0_1>>::getTypeMetadata(), _getOpaquePointer());
// CHECK-NEXT: }
// CHECK-NEXT: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: template<class T_1_0>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_1_0>
// CHECK-NEXT: inline T_1_0 GenericPair<T_0_0, T_0_1>::genericMethod(const T_1_0& x, const T_0_1& y) const {
// CHECK-NEXT: if constexpr (std::is_base_of<::swift::_impl::RefCountedClass, T_1_0>::value) {
// CHECK-NEXT: void *returnValue;
// CHECK-NEXT: _impl::$s8Generics11GenericPairV13genericMethodyqd__qd___q_tlF(reinterpret_cast<void *>(&returnValue), swift::_impl::getOpaquePointer(x), swift::_impl::getOpaquePointer(y), swift::TypeMetadataTrait<GenericPair<T_0_0, T_0_1>>::getTypeMetadata(), swift::TypeMetadataTrait<T_1_0>::getTypeMetadata(), _getOpaquePointer());
// CHECK-NEXT: return ::swift::_impl::implClassFor<T_1_0>::type::makeRetained(returnValue);
// CHECK-NEXT: } else if constexpr (::swift::_impl::isValueType<T_1_0>) {
// CHECK-NEXT: return ::swift::_impl::implClassFor<T_1_0>::type::returnNewValue([&](void * _Nonnull returnValue) {
// CHECK-NEXT: _impl::$s8Generics11GenericPairV13genericMethodyqd__qd___q_tlF(returnValue, swift::_impl::getOpaquePointer(x), swift::_impl::getOpaquePointer(y), swift::TypeMetadataTrait<GenericPair<T_0_0, T_0_1>>::getTypeMetadata(), swift::TypeMetadataTrait<T_1_0>::getTypeMetadata(), _getOpaquePointer());
// CHECK-NEXT: });
// CHECK-NEXT: } else {
// CHECK-NEXT: T_1_0 returnValue;
// CHECK-NEXT: _impl::$s8Generics11GenericPairV13genericMethodyqd__qd___q_tlF(reinterpret_cast<void *>(&returnValue), swift::_impl::getOpaquePointer(x), swift::_impl::getOpaquePointer(y), swift::TypeMetadataTrait<GenericPair<T_0_0, T_0_1>>::getTypeMetadata(), swift::TypeMetadataTrait<T_1_0>::getTypeMetadata(), _getOpaquePointer());
// CHECK-NEXT: return returnValue;
// CHECK-NEXT: }
// CHECK-NEXT: }
// CHECK-NEXT: template<class T_0_0, class T_0_1>
// CHECK-NEXT: requires swift::isUsableInGenericContext<T_0_0> && swift::isUsableInGenericContext<T_0_1>
// CHECK-NEXT: inline swift::Int GenericPair<T_0_0, T_0_1>::getComputedProp() const {
// CHECK-NEXT: return _impl::$s8Generics11GenericPairV12computedPropSivg(swift::TypeMetadataTrait<GenericPair<T_0_0, T_0_1>>::getTypeMetadata(), _getOpaquePointer());
// CHECK-NEXT: }

// CHECK: inline T_0_0 GenericPair<T_0_0, T_0_1>::getComputedVar()
// CHECK: _impl::$s8Generics11GenericPairV11computedVarxvg(reinterpret_cast<void *>(&returnValue), swift::TypeMetadataTrait<GenericPair<T_0_0, T_0_1>>::getTypeMetadata(), _getOpaquePointer());
// CHECK: inline void GenericPair<T_0_0, T_0_1>::setComputedVar(const T_0_0& newValue) {
// CHECK-NEXT: return _impl::$s8Generics11GenericPairV11computedVarxvs(swift::_impl::getOpaquePointer(newValue), swift::TypeMetadataTrait<GenericPair<T_0_0, T_0_1>>::getTypeMetadata(), _getOpaquePointer());
