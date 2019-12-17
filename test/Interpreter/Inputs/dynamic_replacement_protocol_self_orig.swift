protocol View {
  associatedtype Body: View

  var body: Self.Body { get }
}

extension Never: View {
  typealias Body = Never

  var body: Self.Body { fatalError() }
}

struct AnyView: View {
  var body: Never { fatalError() }
}

protocol ViewModelRenderable {
  var view: AnyView { get }
}

extension ViewModelRenderable where Self: SectionModel {
  static func view<Model, MyView: SectionViewModelView>(for model: Model, ofType: MyView.Type) -> AnyView where Model == MyView.BodyViewModel {
    fatalError()
  }
}

protocol SectionViewModelView where Self: View {
  associatedtype BodyViewModel: SectionModel

  init(bodyViewModel: BodyViewModel)
}

public protocol SectionModel: Codable {
  var sectionName: String { get }
}

extension SectionModel {
  public var sectionName: String {
    "Hello world!"
  }
}

struct NewUserModel: SectionModel {
}

extension NewUserModel: ViewModelRenderable {
  var view: AnyView { Self.view(for: self, ofType: NewUserView.self) }
}

struct NewUserView: SectionViewModelView {
  var bodyViewModel: NewUserModel = .init()

  var body: Never {
    fatalError()
  }
}
