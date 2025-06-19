// {"signature":"swift::CanTypeVisitor<swift::TypeMatcher<(anonymous namespace)::AssociatedTypeInference::getPotentialTypeWitnessesByMatchingTypes(swift::ValueDecl*, swift::ValueDecl*)::MatchVisitor>::MatchVisitor, bool, swift::Type, swift::Type>::visit(swift::CanType, swift::Type, swift::Type)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a { associatedtype b func c(_ : _ d: b }
                         extension a { c(_ : _ d: b struct e : a
