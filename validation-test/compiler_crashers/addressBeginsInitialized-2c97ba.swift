// {"kind":"emit-sil","signature":"addressBeginsInitialized(swift::MarkUnresolvedNonCopyableValueInst*)","signatureNext":"MoveOnlyAddressCheckerPImpl::performSingleCheck"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
func a<each b>(c: repeat borrowing each b) {
}
