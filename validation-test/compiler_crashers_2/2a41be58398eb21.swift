// {"signature":"swift::DeclContext::getASTContext() const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@a({                              struct b                 }
                        var c
