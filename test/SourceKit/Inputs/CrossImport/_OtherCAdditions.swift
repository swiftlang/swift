@_exported import Other
@_exported import A
import B
import C

@available(*, unavailable)
public func filtered() {}

/// This has some interesting documentation that shouldn't be separated from
/// the decl when we print the comment detailing its required bystanders in the
/// generated interface of 'Other'.
public func from_OtherCAdditions() {}
