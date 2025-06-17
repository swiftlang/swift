// {"signature":"swift::rewriting::RewriteSystem::processTypeDifference(swift::rewriting::TypeDifference const&, unsigned int, unsigned int, swift::rewriting::RewritePath const&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a < b class c<b, h> : a<(b, h)> protocol d {
  associatedtype b : c<e, f> associatedtype e associatedtype f associatedtype b
      : a<g>
            associatedtype g where g == e
