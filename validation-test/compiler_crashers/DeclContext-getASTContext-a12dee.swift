// {"kind":"typecheck","signature":"swift::DeclContext::getASTContext() const","signatureNext":"computeStorage"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@a({ struct b }
                        var c
