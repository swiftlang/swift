// {"signature":"swift::rewriting::PropertyMap::addSuperclassProperty(swift::rewriting::Term, swift::rewriting::Symbol, unsigned int)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a < b class c : a func d < b where b : a<Int>, b : c
