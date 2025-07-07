// {"signature":"swift::Parser::parseStorageRestrictionsAttribute(swift::SourceLoc, swift::SourceLoc)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@storageRestrictions(
