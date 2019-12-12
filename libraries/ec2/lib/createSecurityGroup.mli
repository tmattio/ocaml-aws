open Types
type input = CreateSecurityGroupRequest.t
type output = CreateSecurityGroupResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error