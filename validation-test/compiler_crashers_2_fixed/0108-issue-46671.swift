// RUN: %target-swift-frontend -emit-ir -primary-file %s

// https://github.com/apple/swift/issues/46671

class UITableViewCell {}
class UITableView {}

extension UITableViewCell: ReusableViewProtocol {
    public typealias ParentView = UITableView
}

protocol ReusableViewProtocol {
    associatedtype ParentView
}

protocol ReusableViewFactoryProtocol {
    associatedtype View: ReusableViewProtocol
    func configure(parentView: View.ParentView)
}

extension ReusableViewFactoryProtocol where View: UITableViewCell {
    func tableCellFor(tableView: UITableView) {
        configure(parentView: tableView)
    }
}
