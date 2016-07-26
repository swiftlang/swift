
@available(OSX 10.11, *)
protocol CNContactPickerDelegate : NSObjectProtocol {
  optional func contactPickerWillClose(_ picker: CNContactPicker)
  optional func contactPickerDidClose(_ picker: CNContactPicker)
}
