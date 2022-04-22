// RUN: %target-swift-frontend -emit-ir %s

public struct Observable<T> {}

public protocol BaseVariant: CaseIterable, Equatable {}

public protocol FeatureGate {
    associatedtype Variant: BaseVariant
}

public enum FeatureVariantState<T: BaseVariant>: Equatable {}

public protocol BaseGatingProvider {
    func exposeFeatureVariantState<G: FeatureGate>(for featureGate: G)
        -> Observable<FeatureVariantState<G.Variant>>
}

public struct UserFeatureGate<Variant: BaseVariant>: FeatureGate {}

public protocol UserGatingProvider: BaseGatingProvider {
    func exposeFeatureVariantState<V>(for featureGate: UserFeatureGate<V>)
    -> Observable<FeatureVariantState<V>>
}
