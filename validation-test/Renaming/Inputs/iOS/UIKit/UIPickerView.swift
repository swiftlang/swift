
@available(iOS 2.0, *)
class UIPickerView : UIView, NSCoding, UITableViewDataSource {
  weak var dataSource: @sil_weak UIPickerViewDataSource?
  weak var delegate: @sil_weak UIPickerViewDelegate?
  var showsSelectionIndicator: Bool
  var numberOfComponents: Int { get }
  @discardableResult
  func numberOfRows(inComponent component: Int) -> Int
  @discardableResult
  func rowSize(forComponent component: Int) -> CGSize
  @discardableResult
  func view(forRow row: Int, forComponent component: Int) -> UIView?
  func reloadAllComponents()
  func reloadComponent(_ component: Int)
  func selectRow(_ row: Int, inComponent component: Int, animated animated: Bool)
  @discardableResult
  func selectedRow(inComponent component: Int) -> Int
}
protocol UIPickerViewDataSource : NSObjectProtocol {
  @available(iOS 2.0, *)
  @discardableResult
  func numberOfComponents(in pickerView: UIPickerView) -> Int
  @available(iOS 2.0, *)
  @discardableResult
  func pickerView(_ pickerView: UIPickerView, numberOfRowsInComponent component: Int) -> Int
}
protocol UIPickerViewDelegate : NSObjectProtocol {
  @available(iOS 2.0, *)
  @discardableResult
  optional func pickerView(_ pickerView: UIPickerView, widthForComponent component: Int) -> CGFloat
  @available(iOS 2.0, *)
  @discardableResult
  optional func pickerView(_ pickerView: UIPickerView, rowHeightForComponent component: Int) -> CGFloat
  @available(iOS 2.0, *)
  @discardableResult
  optional func pickerView(_ pickerView: UIPickerView, titleForRow row: Int, forComponent component: Int) -> String?
  @available(iOS 6.0, *)
  @discardableResult
  optional func pickerView(_ pickerView: UIPickerView, attributedTitleForRow row: Int, forComponent component: Int) -> NSAttributedString?
  @available(iOS 2.0, *)
  @discardableResult
  optional func pickerView(_ pickerView: UIPickerView, viewForRow row: Int, forComponent component: Int, reusing view: UIView?) -> UIView
  @available(iOS 2.0, *)
  optional func pickerView(_ pickerView: UIPickerView, didSelectRow row: Int, inComponent component: Int)
}
