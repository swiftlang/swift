
@available(OSX 10.10, *)
class CKLocationSortDescriptor : NSSortDescriptor, NSSecureCoding {
  init(key key: String, relativeLocation relativeLocation: CLLocation)
  @NSCopying var relativeLocation: CLLocation { get }
}
