open Types
type input = GetPublicAccessBlockRequest.t
type output = GetPublicAccessBlockOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error