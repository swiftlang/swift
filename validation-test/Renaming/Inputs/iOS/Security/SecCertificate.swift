
@available(iOS 2.0, *)
@discardableResult
func SecCertificateGetTypeID() -> CFTypeID
@available(iOS 2.0, *)
@discardableResult
func SecCertificateCreateWithData(_ allocator: CFAllocator?, _ data: CFData) -> SecCertificate?
@available(iOS 2.0, *)
@discardableResult
func SecCertificateCopyData(_ certificate: SecCertificate) -> CFData
@available(iOS 2.0, *)
@discardableResult
func SecCertificateCopySubjectSummary(_ certificate: SecCertificate) -> CFString?
