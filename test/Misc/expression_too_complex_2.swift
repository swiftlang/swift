// RUN: %target-typecheck-verify-swift -solver-memory-threshold 32000
// REQUIRES: OS=ios
import UIKit
class MyViewCell: UITableViewCell {
  let View1 : UIView = UIView(frame: .zero)
  let label1 = UILabel(frame: .zero)
  let label2 = UILabel(frame: .zero)
  let label3 = UILabel(frame: .zero)
  required init?(coder aDecoder: NSCoder) { fatalError("no") }
  override init(style: UITableViewCellStyle, reuseIdentifier: String?) {
    super.init(style: .default, reuseIdentifier: reuseIdentifier)
    NSLayoutConstraint.activate([
        NSLayoutConstraint(item: View1, attribute: .top, relatedBy: .equal, toItem: label1, attribute: .top, multiplier: 1, constant: 1),
        NSLayoutConstraint(item: View1, attribute: .top, relatedBy: .equal, toItem: label2, attribute: .top, multiplier: 1, constant: 1),
        NSLayoutConstraint(item: View1, attribute: .top, relatedBy: .equal, toItem: label3, attribute: .top, multiplier: 1, constant: 1),
        NSLayoutConstraint(item: View1, attribute: .top, relatedBy: .equal, toItem: label1, attribute: .top, multiplier: 1, constant: 1),
        NSLayoutConstraint(item: label1, attribute: .top, relatedBy: .equal, toItem: label2, attribute: .top, multiplier: 1, constant: 1),
        NSLayoutConstraint(item: label1, attribute: .top, relatedBy: .equal, toItem: label3, attribute: .top, multiplier: 1, constant: 1),
        NSLayoutConstraint(item: label1, attribute: .top, relatedBy: .equal, toItem: label1, attribute: .top, multiplier: 1, constant: 1),
        NSLayoutConstraint(item: label1, attribute: .top, relatedBy: .equal, toItem: label2, attribute: .top, multiplier: 1, constant: 1),
        NSLayoutConstraint(item: label2, attribute: .top, relatedBy: .equal, toItem: label3, attribute: .top, multiplier: 1, constant: 1),
        NSLayoutConstraint(item: label2, attribute: .top, relatedBy: .equal, toItem: label1, attribute: .top, multiplier: 1, constant: 1),
        NSLayoutConstraint(item: label3, attribute: .top, relatedBy: .equal, toItem: label2, attribute: .top, multiplier: 1, constant: 1),
        NSLayoutConstraint(item: label3, attribute: .top, relatedBy: .equal, toItem: label3, attribute: .top, multiplier: 1, constant: 1),
        NSLayoutConstraint(item: label3, attribute: .top, relatedBy: .equal, toItem: label2, attribute: .top, multiplier: 1, constant: 1)
      ])
  }
}
