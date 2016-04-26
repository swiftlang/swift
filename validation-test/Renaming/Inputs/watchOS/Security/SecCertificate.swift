
@available(watchOS 2.0, *)
@discardableResult
func SecCertificateGetTypeID() -> CFTypeID
@available(watchOS 2.0, *)
@discardableResult
func SecCertificateCreateWithData(_ allocator: CFAllocator?, _ data: CFData) -> SecCertificate?
@available(watchOS 2.0, *)
@discardableResult
func SecCertificateCopyData(_ certificate: SecCertificate) -> CFData
@available(watchOS 2.0, *)
@discardableResult
func SecCertificateCopySubjectSummary(_ certificate: SecCertificate) -> CFString?
