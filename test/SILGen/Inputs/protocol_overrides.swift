public protocol OriginalGetter {
  associatedtype Index
  associatedtype Element
  
  subscript (index: Index) -> Element { get }
}

public protocol OverridesGetter: OriginalGetter {
  override subscript (index: Index) -> Element { get }
}

public protocol AddsSetter: OverridesGetter {
  override subscript (index: Index) -> Element { get set }
}

public protocol OverridesSetter: AddsSetter {
  override subscript (index: Index) -> Element { get set }
}

