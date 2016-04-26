
@available(iOS 8.0, *)
class CKLocationSortDescriptor : NSSortDescriptor, NSSecureCoding {
  init(key key: String, relativeLocation relativeLocation: CLLocation)
  @NSCopying var relativeLocation: CLLocation { get }
}
