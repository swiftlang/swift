// {"signature":"swift::rewriting::RewriteSystem::verifyMinimizedRules(llvm::DenseSet<unsigned int, llvm::DenseMapInfo<unsigned int, void>> const&) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a: Collection where Element == b!{ associatedtype b: c }
                        protocol d { associatedtype e where e == Self.b.e associatedtype f: a where f.Element == e }
                        protocol c { associatedtype e: g associatedtype h: i where h.b == Self protocol i: d protocol g { associatedtype b: c where b.e == Self
