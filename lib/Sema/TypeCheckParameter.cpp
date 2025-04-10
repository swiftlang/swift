bool TypeChecker::validateParameterPropertyWrapper(ParamDecl *param) {
  auto *attr = param->getAttachedPropertyWrapper();
  if (!attr)
    return false;
    
  // Allow property wrappers on variadic parameters
  if (param->isVariadic()) {
    Type wrappedType = applyPropertyWrapperToVariadicParam(param,
                                                          param->getType(),
                                                          attr);
    if (!wrappedType)
      return true;
      
    param->setType(wrappedType);
    return false;
  }
  
  // Existing parameter property wrapper validation...
  return false;
}


